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
  (let [updated-indices-edge (apply (partial merge {})
                                    (filter identity
                                            (for [[[index port] route-seed] (mapv vector edge (generate-seeds seed (count edge)))]
                                              (if (contains? index-update index)
                                                [(get index-update index) port]
                                                (if (= port :output)
                                                  [:reroute port])))))]
    (if (updated-indices-edge :reroute)
      (assoc (dissoc updated-indices-edge :reroute)
             (pure-rand-nth seed
                            (set/difference (set output-indices)
                                            (set (keys (dissoc updated-indices-edge :reroute)))))
             :output)
      updated-indices-edge)))

(defn find-output-indices [nodes]
  (mapv first (filter (fn [[index node]] (not= (get node :type) :output)) (enumerate nodes))))

(defn common-output [a b]
  (= (->> a set/map-invert :output)
     (->> b set/map-invert :output)))

(defn combine-edges [edges]
  (loop [in-buffer edges
         out-buffer []]
    (if (->> in-buffer empty? not)
      (recur
       (filter #(not (common-output (first in-buffer) %)) (rest in-buffer))
       (->> in-buffer
            (filter (partial common-output (first in-buffer)))
            (apply merge)
            (conj out-buffer)))
      out-buffer)))

(combine-edges [{1 :output 2 :input} {1 :output 10 :clk} {1 :output 2 :clk}])

(defn dumb-crossover [seed [a-nodes a-edges] [b-nodes b-edges]]
  (let [[nodes-seed edges-seed fix-seed] (generate-seeds seed 3)
        b-offset-edges (update-edges (count a-nodes) b-edges)
        sampled-edges (concat a-edges b-offset-edges)
        [sampled-indices sampled-nodes] (->> (concat a-nodes b-nodes)
                                             enumerate
                                             (pure-sample nodes-seed 0.5)
                                             (apply mapv vector))
        index-map (->> sampled-indices
                       enumerate
                       (apply (partial merge {}))
                       set/map-invert)
        edge-fixer (partial fix-edge fix-seed index-map (find-output-indices sampled-nodes))]
    [sampled-nodes (combine-edges (mapv edge-fixer sampled-edges))]))

(defn lossless-crossover [_ [a-nodes a-edges] [b-nodes b-edges]]
  (let [b-offset-edges (update-edges (count a-nodes) b-edges)
        sampled-edges (concat a-edges b-offset-edges)
        sampled-nodes (concat a-nodes b-nodes)]
    [sampled-nodes sampled-edges]))

(defn pad [col size]
  (concat col (repeat (- size (count col)) nil)))

(defn seeded-select [[seed [a b]]]
  (if (= 0 (mod seed 2)) a b))

(defn compatible-crossover [seed [a-nodes a-edges] [b-nodes b-edges]]
  (let [[node-seed edge-seed] (generate-seeds seed 2)
        num-nodes (apply max (map count [a-nodes b-nodes]))
        num-edges (apply max (map count [a-edges b-edges]))
        node-seeds (generate-seeds node-seed num-nodes)
        edge-seeds (generate-seeds edge-seed num-edges)
        node-pairs (map vector (pad a-nodes num-nodes) (pad b-nodes num-nodes))
        edge-pairs (map vector (pad a-edges num-edges) (pad b-edges num-edges))]
    [(->> (map vector node-seeds node-pairs)
          (map (fn [[seed [a b]]] (if (= 0 (mod seed 2)) a b))))
     (->> (map vector edge-seeds edge-pairs)
          (map (fn [[seed [a b]]] (if (= 0 (mod seed 2)) a b))))]))

;;(use 'genetic.representation)
;;
;;;; compatible-crossover of the same individual should return that individual
;;(assert (= (genetic-representation "example.blif.old")
;;           (compatible-crossover 0 (genetic-representation "example.blif.old")
;;                                 (genetic-representation "example.blif.old"))))

(defn check-same-ports [[a b]]
  (and (= (a :type) (b :type))
       (if (= (a :type) :names)
         (= (a :num-inputs) (b :num-inputs))
         true)))

(defn check-compatible [[a-nodes a-edges] [b-nodes b-edges]]
  (every? check-same-ports (map vector a-nodes b-nodes)))

;;(check-compatible (genetic-representation "example.blif.old") (genetic-representation "example.blif.old"))

(defn crossover [[a b]] (dumb-crossover a b))
