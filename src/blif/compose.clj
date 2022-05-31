(ns blif.compose
  (:require [clojure.string :as str])
  (:require [util :refer [input-keyword]]))

(defmacro MISSING-EDGE [] "missing_edge")

(defmacro GENERATED-MODULE-NAME [] "presynth")

(defn edge-to-var [edge]
  {:pre (< (count edge) 3)
   :post (string? %)}
  (format "wire%s" (-> (keys edge)
                    (or ,,, (MISSING-EDGE))
                    pr-str
                    (str/replace ,,, #"\s" "_")
                    (str/replace ,,, #"\(|\)" ""))))

(defn create-var [index port edges]
  (let [check
        (fn [edge]
          (and
           (.contains (keys edge) index)
           (= (get edge index) port)))]
    (let [edge (filter check edges)]
      (if (first edge)
        (edge-to-var (first edge))
        (MISSING-EDGE)))))

(defn generate-input [node index edges]
  (format ".inputs %s" (create-var index :output edges)))

(defn generate-output [node index edges]
  (format ".outputs %s" (create-var index :input edges)))

(defn generate-latch [node index edges]
  (let [input (create-var index :input edges)
        output (create-var index :output edges)
        clk (create-var index :clk edges)
        trigger (get node :trigger-type)
        initial (get node :initial)]
    (format ".latch %s %s %s %s %s" input output trigger clk initial)))

(defn generate-constant [node index edges]
  (let [table (if (= :true (get node :value)) "\n 1" "")]
    (format ".names %s %s" (create-var index :output edges) table)))

(defn generate-names [node index edges]
  (let [input-labels (map input-keyword (range (get node :num-inputs)))]
    (let [args (map #(create-var index % edges) input-labels)]
      (let [output (create-var index :output edges)]
        (format ".names %s %s \n %s"
                (str/join " " args)
                (create-var index :output edges)
                (str/join "\n" (map (partial str/join " ") (get node :table))))))))

(defn generate [node index edges]
  (case (get node :type)
    :input (generate-input node index edges)
    :output (generate-output node index edges)
    :latch (generate-latch node index edges)
    :constant (generate-constant node index edges)
    :names (generate-names node index edges)
    (throw "Unrecognised Node Type encountered during BLIF generation.")))

(defn generate-blif [[nodes edges]]
  (format ".model %s\n%s\n.names %s\n.end"
          (GENERATED-MODULE-NAME)
          (str/join "\n" (map-indexed (fn [index node] (generate node index edges)) nodes))
          (str (MISSING-EDGE))))
