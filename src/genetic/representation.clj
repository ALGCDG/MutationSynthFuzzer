(ns genetic.representation
  (:require [blif.parser :refer [parse]]))

(defn get-vars [ports]
  (let [vars (map keys ports)] (reduce concat vars)))
;; to find edge genes, iterate through variables, find edges where they connect

(defn discover-edge [ports var]
  {:post [(= 1 (count (filter (partial = :output) (vals %))))]}  ;; Post condition, any edge has only one output
  (->> (map-indexed (fn [index port] (if (contains? port var) {index (get port var)})) ports)
       (filter identity)
       (apply merge)))

(defn ports-to-edges [ports]
  (map (partial discover-edge ports) (get-vars ports)))

(defn genetic-representation [f]
  (let [[nodes ports] (parse (slurp f))]
    (let [edges (distinct (ports-to-edges ports))]
      [nodes edges])))
