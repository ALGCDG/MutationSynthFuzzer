(ns blif.verilog
  (:require [clojure.java.shell :refer [sh]])
  (:require [blif.compose :refer [generate-blif]])
  (:require [util :refer [log]]))

(defn CONVERT-ARGS [config blif-file verilog-file]
  (format (if (config :convert-synth)
            "read_blif %s; synth; write_verilog %s"
            "read_blif %s; write_verilog %s")
          blif-file verilog-file))

(defn yosys-convert ;; "Run Yosys subprocess to convert blif file to verilog file."
  [config blif-filename verilog-filename]
  (sh "bash" "-c" (format "%s -p \"%s\""
                          (config :yosys-path)
                          (CONVERT-ARGS config blif-filename verilog-filename))))

(defn blif-to-verilog [config dir name blif]
  (let [blif-file (format "%s/%s.blif" dir name)
        verilog-file (format "%s/%s.v" dir name)]
    (log (format "Writing temporary blif file %s..." blif-file))
    (spit blif-file blif)
    (log (format "Yosys (%s) converting blif file %s to verilog file %s..." (config :yosys-path) blif-file verilog-file))
    (let [convert-result (yosys-convert config blif-file verilog-file)]
      (sh "rm" blif-file)
      (if (not= (:exit convert-result) 0)
        (if (= (:exit convert-result) 1)
          (log "Failed to convert")
          (throw (ex-info "BLIF conversion failure" {:type :convert-fail :conversion convert-result}))))
      verilog-file)))

(defn genetic-to-verilog [config dir g]
  (blif-to-verilog config dir (format "%X" (hash g)) (generate-blif config g)))
