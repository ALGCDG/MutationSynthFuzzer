(ns core
  (:require [blif.compose :refer [generate-blif]])
  (:require [blif.verilog :refer [genetic-to-verilog]])
  (:require [genetic.representation :refer [genetic-representation]])
  (:require [genetic.selection :refer [next-population]])
  (:require [synth :refer [run-synthesis]])
  (:require [util :refer [log]])
  (:require [ramdisk :refer [use-ramdisk]])
  (:require [equivalence :refer [check-equivalence]])
  (:require [coverage :refer [measure-coverage]])
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]]))

(defmacro DISK-SIZE [] (int 200e6))  ;; 200 MB

(defn test-genetic [config g tmpfile]
  (log (format "Testing %s" g))
  (try
    (let [input-verilog-path (->> g eval (genetic-to-verilog (config :yosys-path) tmpfile))
          output-verilog-file (str/replace input-verilog-path #"\.v$" ".post.v")
          [coverage synth-result] (measure-coverage config
                                                    tmpfile
                                                    (partial run-synthesis
                                                             (config :synth)
                                                             (config :synth-path)
                                                             (config :timeout)
                                                             input-verilog-path
                                                             output-verilog-file))]
      (check-equivalence config
                         (eval g)
                         tmpfile
                         input-verilog-path
                         output-verilog-file)
      (sh "rm" input-verilog-path)
      (sh "rm" output-verilog-file)
      [coverage g])
    (catch Exception e
      (case (:type (ex-data e))
        :synth-fail (spit (format "bugs/%X.bug.log" (hash g)) {:input g :error (ex-data e)})
        :equiv-fail (spit (format "bugs/%X.bug.log" (hash g)) {:input g :error (ex-data e)})
        :convert-fail (log (format "Failed to convert %s to verilog" g))
        (log (format "Test Failed: %s, %s" g e))))
    (finally (sh "rm" "-rf" "*" :dir tmpfile))))

(defn dump-genetic-state [population generation-count]
  (log (format "Shutting down, dumping current genetic state of generation %d..."
               generation-count))
  (spit (format "state%d.clj" generation-count)
        population))

(defn fuzz [config corpus tmpfile]
  (loop [current-population (if (config :resume)
                              (do (log (format "Resuming fuzzing from state %s" (config :resume)))
                                  (->> config :resume slurp read-string))
                              {:tested [] :untested corpus})
         generation-count 0
         shutdown-hook nil]
    (log (format "Fuzzing generation %d" generation-count))
    (let [new-shutdown-hook (Thread. (partial dump-genetic-state current-population generation-count))]
      (.addShutdownHook (Runtime/getRuntime) new-shutdown-hook)
      (if shutdown-hook (.removeShutdownHook (Runtime/getRuntime) shutdown-hook))
      (let [error-free-population (try
                                    (->> current-population
                                         :untested
                                         (mapv #(test-genetic config % tmpfile))
                                         (filter identity))
                                    (catch Exception e
                                      (throw e)))]
        (log "Developing next generation...")
        (recur (next-population config (concat (:tested current-population)
                                               error-free-population))
               (+ 1 generation-count)
               new-shutdown-hook)))))

(defn check-file-exists [filepath err-msg]
  (assert (.exists (clojure.java.io/file filepath)) err-msg))

(defn check-file-executable [filepath err-msg]
  (assert (= 0 ((sh "[" "-x" filepath "]") :exit)) err-msg))

(defn check-directory [filepath err-msg]
  (assert (.isDirectory (clojure.java.io/file filepath)) err-msg))

(defn check-contians-blifs [filepath]
  (assert (->> filepath
               clojure.java.io/file
               file-seq
               (filter #(re-matches #".*\.blif$" (str %)))
               empty?
               not)
          (format "Provided directory (%s) does not contain any BLIF files!" filepath)))

(defn check-yosys [filepath]
  (let [version (sh "bash" "-c" (format "%s --version" filepath))]
    (assert (= 0 (version :exit))
            (format "Could not run --version command with yosys (%s)" filepath))
    (assert (re-matches #"(?s)Yosys\s.*" (version :out))
            (format "Executable does not identify as Yosys, outputs: %s" (version :out)))))

(defn check-abc [filepath]
  (let [version (sh "bash" "-c" (format "%s -h" filepath))]
    (assert (= 1 (version :exit))
            "Could not run -h command with abc")
    (assert (re-matches #"(?s)UC\sBerkeley,\sABC\s.*" (version :out))
            (format "Executable does not identify as ABC, outputs: %s" (version :out)))))

(defn -main [config-path]
  (let [config (->> config-path slurp read-string)
        [synth synth-path yosys-path corpus-dir sby-path abc-path src-dir] (map config [:synth :synth-path :yosys-path :corpus-dir :sby-path :abc-path :src-dir])]
  ;; Check provided arguments are valid
  ;; synth is a known synthesizer
    (assert (#{:yosys} (keyword synth)) (format "Unrecognised synthesizer %s!" synth))
  ;; synth-path is a valid filepath to a executable
    (log (format "Checking Synth Under Test path: %s ..." synth-path))
    (check-file-exists synth-path
                       (format "Path to Synth Under Test (%s) is not a valid filepath!" synth-path))
    (check-file-executable synth-path
                           (format "Path to Synth Under Test (%s) is not an executable!" synth-path))
  ;; TODO: Check instrumented and can collect coverage
  ;; yosys-path is a valid filepath to a executable (maybe verify that it really is yosys and that it is correctly insturmented)
    (log (format "Checking Yosys path: %s ..." synth-path))
    (check-file-exists yosys-path
                       (format "Path to Yosys (%s) is not a valid filepath!" yosys-path))
    (check-file-executable yosys-path
                           (format "Path to Yosys (%s) is not an executable!" yosys-path))
    (check-yosys yosys-path)
  ;; corpus-dir, check that it is a valid filepath to a directory (maybe that it contains blif files and that each blif contians one module)
    (log (format "Checking Corpus: %s ..." corpus-dir))
    (check-file-exists corpus-dir
                       (format "Corpus directory (%s) does not exist!" corpus-dir))
    (check-directory corpus-dir
                     (format "Corpus directory path (%s) is not a directory!" corpus-dir))
    (check-contians-blifs corpus-dir)
    #_(check-blifs-contain-one-module)
  ;; Check sby-path
    (log (format "Checking Sby path: %s ..." sby-path))
    (check-file-exists sby-path (format "Path to sby (%s) does not exist!" sby-path))
  ;; Check python3 available
    (assert (->> (sh "bash" "-c" "command -v python3") :exit (= 0)) "Could not find python3 command, required for equivalence check.")
  ;; Check abc-path
    (log (format "Checking ABC path: %s ..." abc-path))
    (check-file-exists abc-path (format "Path to abc (%s) does not exist!" abc-path))
    (check-file-executable abc-path
                           (format "Path to abc (%s) is not an executable!" abc-path))
    (check-abc abc-path)
    (log (format "Reading Corpus: %s..." corpus-dir))
    (let [corpus (->> corpus-dir
                      clojure.java.io/file
                      file-seq
                      (filter #(str/includes? % "blif"))
                      (map str)
                      (mapv (fn [x] `(genetic-representation ~x))))]
      (log "Starting Fuzzing...")
      (use-ramdisk (DISK-SIZE)
                   (partial fuzz
                            (if (config :bounty-file)
                              (do
                                (log (format "Reading bounty file %s..." (config :bounty-file)))
                                (assoc config :bounties (->> config :bounty-file slurp read-string)))
                              config)
                            corpus)))))
