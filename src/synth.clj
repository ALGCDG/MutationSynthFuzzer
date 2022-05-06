(ns synth
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]])
  (:require [util :refer [log]]))

(defmacro TIMEOUT [] 30)

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
    :yosys (format "%s -p \"read_verilog %s; synth; %s\"; write_verilog %s"
                   synth-path
                   input-verilog-file
                   (str/join "; " (YOSYS-OPTIMISAIONS))
                   output-verilog-file)
    (throw (Exception. (format "Incompatable synthesizer %s" synth)))))

(defn run-synthesis [synth synth-path input-verilog-file output-verilog-file]
  (log (format "Synthesizing verilog file %s..." input-verilog-file))
  (sh "bash" "-c" (format  "timeout %s %s >> synth-log.txt 2> /dev/null"
                           (TIMEOUT)
                           (synth-command synth synth-path input-verilog-file output-verilog-file)))
  (log (format "Synthesized verilog file %s, output verilog availabe: %s!" input-verilog-file output-verilog-file)))
