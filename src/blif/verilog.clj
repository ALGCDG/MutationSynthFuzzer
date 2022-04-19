(ns blif.verilog
  (:require [clojure.java.shell :refer [sh]])
  (:require [blif.compose :refer [generate-blif]]))

(defn CONVERT-ARGS [blif-file verilog-file]
  (format "read_blif %s; write_verilog %s" blif-file verilog-file))

(defn yosys-convert ;; "Run Yosys subprocess to convert blif file to verilog file."
  [yosys-path blif-filename verilog-filename]
  (sh "bash" "-c" (format "%s -p \"%s\""
                          yosys-path
                          (CONVERT-ARGS blif-filename verilog-filename))))

(defn blif-to-verilog [yosys-path name blif]
  (let [blif-file (format "%s.blif" name)
        verilog-file (format "%s.v" name)]
    (println (format "Writing temporary blif file %s..." blif-file))
    (spit blif-file blif)
    (println (format "Yosys (%s) converting blif file %s to verilog file %s..." yosys-path blif-file verilog-file))
    (yosys-convert yosys-path blif-file verilog-file)))

(defn genetic-to-verilog [yosys-path g]
  (blif-to-verilog yosys-path (format "%X" (hash g)) (generate-blif g)))