(ns equivalence
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]])
  (:require [blif.compose :refer [create-var]])
  (:require [genetic.representation :refer [genetic-representation]]))

;; Equivalence checking using Sby

;; Sby script

(defn read-formal [file] (format "read -format %s" file))

(defn sby-template [files]
  (format
   (str/join "\n" '("[options]"
                    "multiclock on"
                    "mode prove"
                    "aigsmt none"
                    "[engines]"
                    "abc pdr"
                    "[script]"
                    "%s"
                    "prep -top top"
                    "[files]"
                    "%s"))
   (str/join "\n" (map read-formal files))
   (str/join "\n" files)))

(println (sby-template '("top.v " "other.v " "others.v ")))

(defn run-sby [sby-path]
  (let [config-filepath " config.sby "]
    (spit config-filepath (sby-template [" top.v " " pre.v " " post.v "]))
    (sh " bash " " -c " (format " %s -t %s " sby-path config-filepath))))

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

(println (module "top" '("x" "y")
                 (input "x")
                 (output "y")
                 (instance "presynth" "left" "")
                 (instance "postsynth" "right" "")
                 (always "posedge" "clk"
                         (vassert "x == y"))))

(defn assert-equivalent [[siga sigb]] (vassert (format "%s == %s" siga sigb)))

(defn top [[nodes edges]]
  (let [indexed-nodes (map-indexed (fn [index node] (merge node {:index index})) nodes)
        sut-inputs (->> indexed-nodes
                        (filter #(= (:type %) :input))
                        (map :index)
                        (map #(create-var % :output edges))
                        (map #(format "\\%s" %)))
        sut-outputs (->> indexed-nodes
                         (filter #(= (:type %) :output))
                         (map :index)
                         (map #(create-var % :input edges))
                         (map #(format "\\%s" %)))
        top-inputs (take (count sut-inputs) (map #(format "x_%s" %) (range)))
        top-pre-outputs (take (count sut-outputs) (map #(format "y_pre_%s" %) (range)))
        top-post-outputs (take (count sut-outputs) (map #(format "y_post_%s" %) (range)))]
    (module "top" (concat top-inputs top-pre-outputs top-post-outputs)
            (str/join "\n" (map input top-inputs))
            (str/join "\n" (map output (concat top-pre-outputs top-post-outputs)))
            (instance "presynth" "presynth"
                      (merge
                       (zipmap sut-inputs top-inputs)
                       (zipmap sut-outputs top-pre-outputs)))
            (instance "postsynth" "postsynth"
                      (merge
                       (zipmap sut-inputs top-inputs)
                       (zipmap sut-outputs top-post-outputs)))
            (always "posedge" "clk"
                    (->> (map vector top-pre-outputs top-post-outputs)
                         (map assert-equivalent)
                         (str/join "\n"))))))

(println (top (genetic-representation "example.blif.old")))