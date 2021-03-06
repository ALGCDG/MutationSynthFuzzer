(ns blif.verilog
  (:require [clojure.java.shell :refer [sh]])
  (:require [blif.compose :refer [generate-blif]])
  (:require [util :refer [log]]))

(defn CONVERT-ARGS [blif-file verilog-file]
  (format "read_blif %s; write_verilog %s" blif-file verilog-file))

(defn yosys-convert ;; "Run Yosys subprocess to convert blif file to verilog file."
  [yosys-path blif-filename verilog-filename]
  (sh "bash" "-c" (format "%s -p \"%s\" >> convert-log.txt 2> /dev/null"
                          yosys-path
                          (CONVERT-ARGS blif-filename verilog-filename))))

(defn blif-to-verilog [yosys-path dir name blif]
  (let [blif-file (format "%s/%s.blif" dir name)
        verilog-file (format "%s/%s.v" dir name)]
    (log (format "Writing temporary blif file %s..." blif-file))
    (spit blif-file blif)
    (log (format "Yosys (%s) converting blif file %s to verilog file %s..." yosys-path blif-file verilog-file))
    (let [convert-result (yosys-convert yosys-path blif-file verilog-file)]
      (sh "rm" blif-file)
      (if (not= (:exit convert-result) 0)
        (throw (ex-info "BLIF conversion failure" {:type :convert-fail})))
      verilog-file)))

(defn genetic-to-verilog [config dir g]
  (blif-to-verilog (config :yosys-path) dir (format "%X" (hash g)) (generate-blif config g)))
