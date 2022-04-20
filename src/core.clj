(ns core
  (:require [clojure.java.shell :refer [sh]]))

(defmacro DISK-SIZE [] 200e6)  ;; 200 MB

#_(defn fuzz [synth synth-path yosys-path corpus tmpfile])

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
  (println "Starting Fuzzing...")
  #_(use-ramdisk 200e6 (partial fuzz)))
