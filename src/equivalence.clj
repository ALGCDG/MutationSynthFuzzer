(ns equivalence
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]])
  (:require [blif.compose :refer [create-var GENERATED-MODULE-NAME MODULE-CLK]])
  (:require [synth :refer [SYNTHED-MODULE-NAME]])
  (:require [util :refer [log]])
  (:require [genetic.representation :refer [genetic-representation]]))

;; Equivalence checking using Sby

;; Sby script

(defn read-formal [file] (format "read -formal %s" file))

(defn sby-template [files full-trace]
  (format
   (str/join "\n" ["[options]"
                   "multiclock on"
                   "mode prove"
                   (if full-trace "" "aigsmt none")
                   "[engines]"
                   "abc pdr"
                   "[script]"
                   "%s"
                   "prep -top top"
                   "[files]"
                   "%s"])
   (str/join "\n" (map read-formal files))
   (str/join "\n" files)))

;;(println (sby-template '("top.v " "other.v " "others.v ")))

(defmacro PROOF-NAME [] "equiv_check")
(defmacro PROOF-TIMEOUT [] 3600)  ;; Default proof timeout, 1 hour

(defn run-sby [config tmpfile top-path pre-synth-path post-synth-path full-trace]
  (let [config-filepath (format "%s/%s_%s.sby" tmpfile (or (config :id) "") (PROOF-NAME))]
    (spit config-filepath (sby-template [top-path pre-synth-path post-synth-path] full-trace))
    (let [sby-command (format "timeout %s %s %s --yosys %s --abc %s %s -f %s"
                              (or (config :proof-timeout) (PROOF-TIMEOUT))
                              (config :python)
                              (config :sby-path)
                              (config :yosys-path)
                              (config :abc-path)
                              (if (config :smtbmc-path) (format "--smtbmc %s" (config :smtbmc-path)) "")
                              config-filepath)]
      (println sby-command)
      (sh "bash" "-c" (format "timeout %s %s %s --yosys %s --abc %s %s -f %s"
                              (or (config :proof-timeout) (PROOF-TIMEOUT))
                              (config :python)
                              (config :sby-path)
                              (config :yosys-path)
                              (config :abc-path)
                              (if (config :smtbmc-path) (format "--smtbmc %s" (config :smtbmc-path)) "")
                              config-filepath)
          :dir tmpfile))))

;; Verilog Top File Templating

(defn module [name ports & body]
  (format "module %s (%s);\n%s\nendmodule" name (str/join ", " ports) (str/join "\n" body)))

(defn always [trigger-type trigger & body]
  (format "always @(%s %s) begin\n%s\nend" trigger-type trigger (str/join "\n" body)))

(defn vassert [condition] (format "assert (%s);" condition))

(defn input [variable] (format "input %s;" variable))

(defn output [variable] (format "output %s;" variable))

(defn instance [module name params]
  (format "%s %s (%s);"
          module
          name
          (str/join ", " (for [[sut-arg top-arg] params] (format ".%s (%s)" sut-arg top-arg)))))

;;(println (module "top" '("x" "y")
;;                 (input "x")
;;                 (output "y")
;;                 (instance "presynth" "left" "")
;;                 (instance "postsynth" "right" "")
;;                 (always "posedge" "clk"
;;                         (vassert "x == y"))))

(defn assert-equivalent [[sig-pre sig-post]] (vassert (format "$isunknown(%s) || (%s == %s)"
                                                              sig-pre
                                                              sig-pre
                                                              sig-post)))

(defmacro TOP-CLK [] "clk")

(defn top [config [nodes edges]]
  (let [indexed-nodes (map-indexed (fn [index node] (merge node {:index index})) nodes)
        sut-inputs (->> indexed-nodes
                        (filter #(= (:type %) :input))
                        (map :index)
                        (map #(create-var % :output edges))
                        (map #(format "%s" %)))
        sut-outputs (->> indexed-nodes
                         (filter #(= (:type %) :output))
                         (map :index)
                         (map #(create-var % :input edges))
                         (map #(format "%s" %)))
        top-inputs (take (count sut-inputs) (map #(format "x_%s" %) (range)))
        top-pre-outputs (distinct (map #(format "y_pre_%s" %) sut-outputs))
        top-post-outputs (distinct (map #(format "y_post_%s" %) sut-outputs))]
    (module "top" (concat [(TOP-CLK)] top-inputs top-pre-outputs top-post-outputs)
            (input (TOP-CLK))
            (str/join "\n" (map input top-inputs))
            (str/join "\n" (map output (concat top-pre-outputs top-post-outputs)))
            (instance (GENERATED-MODULE-NAME) "pre"
                      (merge
                       (if (config :force-clk)
                         {(MODULE-CLK) (TOP-CLK)}
                         {})
                       (zipmap sut-inputs top-inputs)
                       (zipmap sut-outputs top-pre-outputs)))
            (instance (SYNTHED-MODULE-NAME) "post"
                      (merge
                       (if (config :force-clk)
                         {(MODULE-CLK) (TOP-CLK)}
                         {})
                       (zipmap sut-inputs top-inputs)
                       (zipmap sut-outputs top-post-outputs)))
            (always "posedge" (TOP-CLK)
                    (->> (map vector top-pre-outputs top-post-outputs)
                         (map assert-equivalent)
                         (str/join "\n"))))))

;;(println (top {} (genetic-representation "example.blif.old")))

(defn build-sim [& files]
  (sh "bash" "-c" (format "iverilog -g2012 %s" (str/join " " files))))

(defn run-sim []
  (sh "bash" "-c" "vvp a.out"))

(defn confirm-with-simrun [base-error sim-run-result]
  (if (not (empty? (sim-run-result :out)))
    (throw (ex-info "Equivalence Proof Failed, Simulation Agrees"
                    (merge base-error
                           {:sim-run sim-run-result})))
    (log "Simulation failed to confirm proof failure.")))

(defn confirm-with-simbuild [base-error & files]
  (let [sim-compile-result (apply build-sim files)
        elaborated-error (merge base-error {:sim-compile sim-compile-result})]
    (if (= (sim-compile-result :exit) 0)
      (confirm-with-simrun elaborated-error (run-sim))
      (throw (ex-info "Equivalence Proof Failed, could not simulate counter example."
                      elaborated-error)))))

(defn confirm-bug [tmpfile base-error & files]
  (if (re-find #"trace_tb.v" (-> base-error :proof :out))
    (let [counter-eg-tb-path (format "%s/%s_%s/engine_0/trace_tb.v"
                                     tmpfile
                                     ""
                                     (PROOF-NAME))]
      (apply (partial confirm-with-simbuild base-error counter-eg-tb-path) files))
    (throw (ex-info "Equivalence Proof Failed" base-error))))

(defn check-equivalence [config g tmpfile pre-synth-path post-synth-path]
  (let [top-path (format "%s/top.v" tmpfile)]
    (spit top-path (top config g))
    (let [proof-result (run-sby config tmpfile top-path pre-synth-path post-synth-path true)]
      (case (:exit proof-result)
        0 (log "Succesfully Proved Equivalent")
        124 (log "Equivalence Proof Timedout")
        (confirm-bug tmpfile
                     {:type :equiv-fail
                      :pre-synth-verilog (slurp pre-synth-path)
                      :post-synth-verilog (slurp post-synth-path)
                      :proof proof-result}
                     top-path
                     pre-synth-path
                     post-synth-path)))))

(defn simulate [config g tmpfile pre-synth-path post-synth-path]
  (let [top-path (format "%s/top.v" tmpfile)]
    (spit top-path (top config g))
    (let [proof-result (run-sby config tmpfile top-path pre-synth-path post-synth-path true)]
      (if (re-find #"trace_tb.v" (proof-result :out))
        (let [counter-eg-tb-path (format "%s/%s_%s/engine_0/trace_tb.v" tmpfile (or (config :id) "") (PROOF-NAME))
              sim-compile-result (sh "bash" "-c" (format "iverilog -g2012 %s %s %s %s"
                                                         top-path
                                                         pre-synth-path
                                                         post-synth-path
                                                         counter-eg-tb-path))
              sim-run-result (sh "bash" "-c" "vvp a.out")]
          {:sim-run sim-run-result :sim-compile sim-compile-result})
        (println "Couldn't find trace_tb.v" (proof-result :out))))))
