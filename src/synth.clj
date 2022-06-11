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
(def quartus-sdc "create_clock -period 5 -name clk [get_ports clock]")

(defn quartus-config [module-name file-path sdc-path]
  (str/join "\n"
            ["load_package flow"
             ""
             (format "project_new -overwrite %s" module-name)
             ""
             "set_global_assignment -name FAMILY \"Cyclone 10 GX\""
             (format "set_global_assignment -name SYSTEMVERILOG_FILE %s" file-path)
             (format "set_global_assignment -name TOP_LEVEL_ENTITY %s" module-name)
             (format "set_global_assignment -name SDC_FILE %s" sdc-path)
             "set_global_assignment -name INI_VARS \"qatm_force_vqm=on;\""
             "set_global_assignment -name NUM_PARALLEL_PROCESSORS 2"
             "set_instance_assignment -name VIRTUAL_PIN ON -to *"
             ""
             "execute_module -tool syn"
             "execute_module -tool eda -args \"--simulation --tool=vcs\""
             ""
             "project_close"]))

(println (quartus-config "top" "/home/vagrant/example.v" "/home/vagrant/example.sdc"))

(defn synth-command [synth synth-path input-verilog-file output-verilog-file]
  (case synth
    :yosys (format "%s -p \"read_verilog %s; synth; %s; rename %s %s; write_verilog %s\""
                   synth-path
                   input-verilog-file
                   (str/join "; " (YOSYS-OPTIMISAIONS))
                   (GENERATED-MODULE-NAME)
                   (SYNTHED-MODULE-NAME)
                   output-verilog-file)
    :quartus (do
               (spit ".sdc" quartus-sdc)
               (spit ".tcl" quartus-config "pre" )
               (format "%s -t %s" synth-path ))
    :evil (format "java -jar %s %s %s" synth-path input-verilog-file output-verilog-file)
    (throw (Exception. (format "Incompatable synthesizer %s" synth)))))

(defn run-synthesis [synth synth-path timeout input-verilog-file output-verilog-file]
  (log (format "Synthesizing verilog file %s..." input-verilog-file))
  (let [synth-result (sh "bash" "-c" (format  "timeout %s %s"
                                              (or timeout (TIMEOUT))
                                              (synth-command synth synth-path input-verilog-file output-verilog-file)))]
    (if (not= (synth-result :exit) 0)
      (if (and (= (synth-result :exit) 124)
               (= (synth-result :err) ""))
        (log ("Synthesis timed out after %ss" timeout))

        (throw (ex-info "Synthesis Failed" {:type :synth-fail :input-verilog (slurp input-verilog-file) :result synth-result}))))
    (log (format "Successfully Synthesized verilog file %s, output verilog availabe: %s!" input-verilog-file output-verilog-file))))
