(ns synth
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]]))

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

(defn synth-command [synth synth-path verilog-file]
  (case synth
    :yosys (format "%s -p \"read_verilog %s; synth; %s\""
                   synth-path
                   verilog-file
                   (str/join "; " (YOSYS-OPTIMISAIONS)))
    (throw (format "Incompatable synthesizer %s" synth))))

(defn run-synthesis [synth synth-path verilog-file]
  (println (format "Synthesizing verilog file %s..." verilog-file))
  (sh "bash" "-c" (format  "timeout %s %s"
                           (TIMEOUT)
                           (synth-command synth synth-path verilog-file))))
