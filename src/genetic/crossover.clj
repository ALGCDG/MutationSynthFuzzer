(ns genetic.crossover
  (:require [clojure.set :as set])
  (:require [util :refer [enumerate]]))

(defn update-edge [offset edge]
  (zipmap (map #(+ offset %) (keys edge)) (vals edge)))
;; TODO, implement properly, may require going back to modify edge representation (replacing one key with another is annoying

(defn update-edges [offset edges]
  (map (partial update-edge offset) edges))

(defn fix-edge [index-update output-indices edge]
  (apply (partial merge {}) (for [[index port] edge]
                              (if (contains? index-update index)
                                [(get index-update index) port]
                                (if (= port :output)
                                  [(rand-nth output-indices) port]
                                  [:missing port])))))

(defn find-output-indices [nodes]
  (map first (filter (fn [[index node]] (not= (get node :type) :output)) (enumerate nodes))))

(defn dumb-crossover [[a-nodes a-edges] [b-nodes b-edges]]
  (let [b-offset-edges (update-edges (count a-nodes) b-edges)
        sampled-edges (random-sample 0.5 (concat a-edges b-offset-edges))
        [sampled-indices sampled-nodes] (->> (concat a-nodes b-nodes)
                                             enumerate
                                             (random-sample 0.5)
                                             (apply map vector))
        index-map (->> sampled-indices
                       enumerate
                       (apply (partial merge {}))
                       set/map-invert)
        edge-fixer (partial fix-edge index-map (find-output-indices sampled-nodes))]
    [sampled-nodes (map edge-fixer sampled-edges)]))

(defn crossover [[a b]] (dumb-crossover a b))
