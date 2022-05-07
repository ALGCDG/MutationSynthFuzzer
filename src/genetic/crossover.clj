(ns genetic.crossover
  (:require [clojure.set :as set])
  (:require [util :refer [enumerate]])
  (:require [pure-random :refer [generate-seeds pure-rand-nth pure-sample]]))

(defn update-edge [offset edge]
  (zipmap (map #(+ offset %) (keys edge)) (vals edge)))
;; TODO, implement properly, may require going back to modify edge representation (replacing one key with another is annoying

(defn update-edges [offset edges]
  (map (partial update-edge offset) edges))

(defn fix-edge [seed index-update output-indices edge]
  (apply (partial merge {}) (for [[[index port] route-seed] (mapv vector edge (generate-seeds seed (count edge)))]
                              (if (contains? index-update index)
                                [(get index-update index) port]
                                (if (= port :output)
                                  [(pure-rand-nth route-seed output-indices) port]
                                  [:missing port])))))

(defn find-output-indices [nodes]
  (mapv first (filter (fn [[index node]] (not= (get node :type) :output)) (enumerate nodes))))

(defn dumb-crossover [seed [a-nodes a-edges] [b-nodes b-edges]]
  (let [[nodes-seed edges-seed fix-seed] (generate-seeds seed 3)
        b-offset-edges (update-edges (count a-nodes) b-edges)
        sampled-edges (pure-sample edges-seed 0.5 (concat a-edges b-offset-edges))
        [sampled-indices sampled-nodes] (->> (concat a-nodes b-nodes)
                                             enumerate
                                             (pure-sample nodes-seed 0.5)
                                             (apply mapv vector))
        index-map (->> sampled-indices
                       enumerate
                       (apply (partial merge {}))
                       set/map-invert)
        edge-fixer (partial fix-edge fix-seed index-map (find-output-indices sampled-nodes))]
    [sampled-nodes (mapv edge-fixer sampled-edges)]))

(defn crossover [[a b]] (dumb-crossover a b))
