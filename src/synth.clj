(ns synth
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]])
  (:require [util :refer [log]])
  (:require [blif.compose :refer [GENERATED-MODULE-NAME]]))

(defmacro TIMEOUT [] 30)

(defmacro SYNTHED-MODULE-NAME [] "postsynth")

(defmacro YOSYS-OPTIMISAIONS [] ["opt"
                                 "opt_clean"
                                 "opt_demorgan"
                                 "opt_expr"
                                 "opt_lut"
                                 "opt_merge"
                                 "opt_muxtree"
                                 "opt_reduce"
                                 "fsm_opt"
                                 "onehot"])

(defn synth-command [synth synth-path input-verilog-file output-verilog-file]
  (case synth
    :yosys (format "%s -p \"read_verilog %s; synth; %s; rename %s %s; write_verilog %s\""
                   synth-path
                   input-verilog-file
                   (str/join "; " (YOSYS-OPTIMISAIONS))
                   (GENERATED-MODULE-NAME)
                   (SYNTHED-MODULE-NAME)
                   output-verilog-file)
    (throw (Exception. (format "Incompatable synthesizer %s" synth)))))

(defn run-synthesis [synth synth-path timeout input-verilog-file output-verilog-file]
  (log (format "Synthesizing verilog file %s..." input-verilog-file))
  (let [synth-result (sh "bash" "-c" (format  "timeout %s %s"
                                              (or timeout (TIMEOUT))
                                              (synth-command synth synth-path input-verilog-file output-verilog-file)))]
    (if (not= (synth-result :exit) 0)
      (throw (ex-info "Synthesis Failed" {:type :synth-fail :input-verilog (slurp input-verilog-file) :result synth-result}))))
  (log (format "Successfully Synthesized verilog file %s, output verilog availabe: %s!" input-verilog-file output-verilog-file)))
