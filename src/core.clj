(ns core
  (:require [blif.compose :refer [generate-blif]])
  (:require [blif.verilog :refer [genetic-to-verilog]])
  (:require [genetic.representation :refer [genetic-representation]])
  (:require [genetic.selection :refer [next-population]])
  (:require [synth :refer [run-synthesis]])
  (:require [util :refer [log]])
  (:require [ramdisk :refer [use-ramdisk]])
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]]))

(defmacro DISK-SIZE [] (int 200e6))  ;; 200 MB

(defn test-genetic [synth synth-path yosys-path g tmpfile]
  (log (format "Testing %s" g))
  (try
    (let [input-verilog-path (->> g eval (genetic-to-verilog yosys-path tmpfile))
          output-verilog-file (str/replace input-verilog-path #"\.v$" ".post.v")
          synth-result (run-synthesis synth synth-path input-verilog-path output-verilog-file)]
      #_(check-equivalence syb-path g input-verilog-path output-verilog-file)
      #_(collect-coverage)
      (sh "rm" input-verilog-path)
      (sh "rm" output-verilog-file)
      g)
    (catch Exception e
      (case (:type (ex-data e))
        :synth-fail (spit (format "bugs/%X.bug.log" (hash g)) {:input g :error (ex-data e)})
        :equiv-fail (spit (format "bugs/%X.bug.log" (hash g)) {:input g :error (ex-data e)})
        :convert-fail (log (format "Failed to convert %s to verilog" g))
        (log (format "Test Failed: %s, %s" g e))))))

(defn dump-genetic-state [population generation-count]
  (log (format "Shutting down, dumping current genetic state of generation %d..."
               generation-count))
  (spit (format "state%d.clj" generation-count)
        population))

(defn fuzz [synth synth-path yosys-path corpus tmpfile]
  (loop [current-population {:tested [] :untested corpus}
         generation-count 0
         shutdown-hook nil]
    (log (format "Fuzzing generation %d" generation-count))
    (let [new-shutdown-hook (Thread. (partial dump-genetic-state current-population generation-count))]
      (.addShutdownHook (Runtime/getRuntime) new-shutdown-hook)
      (if shutdown-hook (.removeShutdownHook (Runtime/getRuntime) shutdown-hook))
      (let [error-free-population (try
                                    (->> current-population
                                         :untested
                                         (mapv #(test-genetic synth synth-path yosys-path % tmpfile))
                                         (filter identity))
                                    (catch Exception e
                                      (throw e)))]
        (log "Developing next generation...")
        (recur (next-population (concat (:tested current-population)
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
            "Could not run --version command with yosys")
    (assert (re-matches #"(?s)Yosys\s.*" (version :out))
            (format "Executable does not identify as Yosys, outputs: %s" (version :out)))))

(defn -main [synth synth-path yosys-path corpus-dir & others]
  ;; Check provided arguments are valid
  ;; synth is a known synthesizer
  (assert (#{:yosys} (keyword synth)) (format "Unrecognised synthesizer %s!" synth))
  ;; synth-path is a valid filepath to a executable
  (println (format "Checking Synth Under Test path: %s ..." synth-path))
  (check-file-exists synth-path
                     (format "Path to Synth Under Test (%s) is not a valid filepath!" synth-path))
  (check-file-executable synth-path
                         (format "Path to Synth Under Test (%s) is not an executable!" synth-path))
  ;; TODO: Check instrumented and can collect coverage
  ;; yosys-path is a valid filepath to a executable (maybe verify that it really is yosys and that it is correctly insturmented)
  (println (format "Checking Yosys path: %s ..." synth-path))
  (check-file-exists yosys-path
                     (format "Path to Yosys (%s) is not a valid filepath!" yosys-path))
  (check-file-executable yosys-path
                         (format "Path to Yosys (%s) is not an executable!" yosys-path))
  (check-yosys yosys-path)
  ;; corpus-dir, check that it is a valid filepath to a directory (maybe that it contains blif files and that each blif contians one module)
  (println (format "Checking Corpus: %s ..." corpus-dir))
  (check-file-exists corpus-dir
                     (format "Corpus directory (%s) does not exist!" corpus-dir))
  (check-directory corpus-dir
                   (format "Corpus directory path (%s) is not a directory!" corpus-dir))
  (check-contians-blifs corpus-dir)
  #_(check-blifs-contain-one-module)
  (println (format "Reading Corpus: %s..." corpus-dir))
  (let [corpus (->> corpus-dir
                    clojure.java.io/file
                    file-seq
                    (filter #(str/includes? % "blif"))
                    (map str)
                    (mapv (fn [x] `(genetic-representation ~x))))]
    (println "Starting Fuzzing...")
    (use-ramdisk (DISK-SIZE) (partial fuzz (keyword synth) synth-path yosys-path corpus))))
