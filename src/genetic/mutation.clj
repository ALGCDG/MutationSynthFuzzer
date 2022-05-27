(ns genetic.mutation
  (:require [clojure.set :as set])
  (:require [util :refer [enumerate input-keyword]])
  (:require [pure-random :refer [pure-rand-nth pure-sample generate-seeds]]))

(defmacro CNF-SYMBOLS [] ["0" "1" "-"])

(defn find-nodes-indexed [nodes type]
  (filterv (fn [[index node]] (= (get node :type) type)) (enumerate nodes)))

(defn rand-nodetype [seed nodes type] (pure-rand-nth seed (find-nodes-indexed nodes type)))

(defn change-latch-trigger [seed [nodes edges]]
  (let [[node-seed trigger-seed] (generate-seeds seed 2)]
    (let [[modified-index node] (rand-nodetype node-seed nodes :latch)]
      (let [other-triggers (set/difference #{:re, :fe, :as, :ah, :al} #{(get node :trigger-type)})]
        (let [new-node (assoc node :trigger-type (pure-rand-nth trigger-seed (into [] other-triggers)))]
          [(assoc nodes modified-index new-node) edges])))))

(defn change-latch-initial [seed [nodes edges]]
  (let [[node-seed initial-seed] (generate-seeds seed 2)]
    (let [[modified-index node] (rand-nodetype node-seed nodes :latch)]
      (let [other-initial (set/difference (set (range 4)) #{(get node :initial)})]
        (let [new-node (assoc node :initial (pure-rand-nth initial-seed (into [] other-initial)))]
          [(assoc nodes modified-index new-node) edges])))))

(defn change-constant-value [seed [nodes edges]]
  (let [[modified-index node] (rand-nodetype seed nodes :constant)]
    (let [new-node (assoc node :value (if (= (get node :value) :true) :false :true))]
      [(assoc nodes modified-index new-node) edges])))

(defn change-names-flip-term [seed [nodes edges]]
  (let [[node-seed row-seed symbol-seed] (generate-seeds seed 3)
        indexed-names-nodes (filter (fn [[index node]] (= (get node :type) :names))
                                    (enumerate nodes))
        [modified-index node] (pure-rand-nth node-seed indexed-names-nodes)
        original-table (node :table)
        [target-row-index target-row] (->> original-table enumerate (pure-rand-nth row-seed))
        [target-symbol-index target-symbol] (->> target-row enumerate (pure-rand-nth symbol-seed))
        new-table (assoc (into [] original-table)
                         target-row-index
                         (assoc target-row
                                target-symbol-index
                                (set/difference (set (CNF-SYMBOLS)) #{target-symbol})))
        new-node (assoc node :table new-table)]
    [(assoc nodes modified-index new-node) edges]))

(defn change-names-remove-clause [seed [nodes edges]]
  (let [[modified-index node] (rand-nodetype seed nodes :names)]
    (let [original-table (get node :table)]
      (let [new-table (rest (shuffle original-table))]
        (let [new-node (assoc node :table new-table)]
          [(assoc nodes modified-index new-node) edges])))))

(defn change-names-add-clause [seed [nodes edges]]
  (let [[node-seed sample-seed] (generate-seeds seed 2)]
    (let [[modified-index node] (rand-nodetype node-seed nodes :names)]
      (let [original-table (get node :table)]
        (let [n (+ 1 (get node :num-inputs))]
          (let [new-clause (take n (pure-sample sample-seed 0.1 (cycle (CNF-SYMBOLS))))]  ;; Note that the probability 0.1 is arbitrary, we are performing a uniform sample (just need to make sure probability is not 1).
            (let [new-table (conj original-table [new-clause])]
              (let [new-node (assoc node :table new-table)]
                [(assoc nodes modified-index new-node) edges]))))))))

(defn cut [index col]
  (concat (take index col) (take-last (- (count col) index 1) col)))

(defn change-names-remove-input [seed [nodes edges]]
  (let [[node-seed input-seed] (generate-seeds seed 2)
        [target-index target-node] (rand-nodetype node-seed nodes :names)
        target-input (->> target-node :num-inputs range (pure-rand-nth input-seed))]
    [(assoc nodes
            target-index
            (assoc target-node
                   :num-inputs (-> target-node :num-inputs (- 1))
                   :table (->> target-node
                               :table
                               (mapv (partial cut target-input)))))
     (let [[affected-index affected-edge] (->> edges
                                               enumerate
                                               (filter (fn [x] (->> x
                                                                    second
                                                                    (#(% target-index))
                                                                    (= (input-keyword target-input)))))
                                               first)]
       (assoc (into [] edges)
              affected-index
              (dissoc affected-edge target-index)))]))

;; add input can use an existing output, or create a new input
(defn change-names-add-input [seed [nodes edges]]
  (let [[node-seed symbol-seed edge-seed] (generate-seeds seed 3)
        [target-index target-node] (rand-nodetype node-seed nodes :names)]
    [(assoc nodes
            target-index
            (assoc target-node
                   :num-inputs (->> target-node :num-inputs (+ 1))
                   :table (->> target-node
                               :table
                               (mapv vector (pure-sample symbol-seed 0.1 (cycle (CNF-SYMBOLS))))
                               (mapv #(conj (second %) (first %))))))
     (let [[edge-index target-edge] (pure-rand-nth edge-seed (->> edges
                                                                  enumerate
                                                                  (filter #(->> % second set/map-invert :output))))]
       (assoc (into [] edges)
              edge-index
              (assoc target-edge target-index (->> target-node :num-inputs input-keyword))))]))

;; add output can use an existing output, or create a new input
(defn add-output [seed [nodes edges]]
  [(conj nodes {:type :output})
   (let [[target-index target-edge] (pure-rand-nth seed
                                                   (->> edges
                                                        enumerate
                                                        (filter #(->> % second set/map-invert :output))))]
     (assoc (into [] edges)
            target-index
            (assoc target-edge (count nodes) :input)))])

(defn edge-switch-source [seed [nodes edges]]
  (let [[seed-a seed-b] (generate-seeds seed 2)
        [index-a edge-a] (pure-rand-nth seed-a (enumerate edges))
        [index-b edge-b] (pure-rand-nth seed-b (enumerate edges))
        source-index-a (->> edge-a set/map-invert :output)
        source-index-b (->> edge-b set/map-invert :output)]
    [nodes
     (assoc (into [] edges)
            index-a
            (assoc (dissoc edge-a source-index-a) source-index-b :output)
            index-b
            (assoc (dissoc edge-b source-index-b) source-index-a :output))]))

(defn add-input [seed [nodes edges]]
  (let [[edge-seed sink-seed] (generate-seeds seed 2)
        [index edge] (pure-rand-nth edge-seed
                                    (filter (fn [[index edge]]
                                              (->> edge
                                                   vals
                                                   (filter #(not= % :output))
                                                   count
                                                   (< 1)))
                                            (enumerate edges)))
        [stolen-sink-index stolen-sink-port] (pure-rand-nth sink-seed
                                                            (filter (fn [[index port]]
                                                                      (not= port :output))
                                                                    edge))]
    [(conj nodes {:type :input})
     (if (and index (integer? index))
       (conj (assoc (into [] edges)
                    index
                    (dissoc edge stolen-sink-index))
             {(count nodes) :output stolen-sink-index stolen-sink-port})
       (conj edges {(count nodes) :output}))]))

(defn edge-steal-sink [seed [nodes edges]]
  (let [[target-seed theif-seed sink-seed] (generate-seeds seed 3)
        [theif-index theif-edge] (pure-rand-nth theif-seed (enumerate edges))
        [target-index target-edge] (pure-rand-nth target-seed
                                                  (filter (fn [[index edge]]
                                                            (->> edge
                                                                 vals
                                                                 (filter #(not= % :output))
                                                                 count
                                                                 (< 1)))
                                                          (enumerate edges)))
        [stolen-sink-index stolen-sink-port] (pure-rand-nth sink-seed
                                                            (filter (fn [[index port]]
                                                                      (not= port :output))
                                                                    target-edge))]
    [nodes
     (assoc (into [] edges)
            theif-index
            (assoc theif-edge stolen-sink-index stolen-sink-port) target-index
            (dissoc target-edge stolen-sink-index))]))

(defn add-constant [seed [nodes edges]]
  (let [[edge-seed sink-seed value-seed] (generate-seeds seed 3)
        [index edge] (pure-rand-nth edge-seed
                                    (filter (fn [[index edge]]
                                              (->> edge
                                                   vals
                                                   (filter #(not= % :output))
                                                   count
                                                   (< 1)))
                                            (enumerate edges)))
        [stolen-sink-index stolen-sink-port] (pure-rand-nth sink-seed
                                                            (filter (fn [[index port]]
                                                                      (not= port :output))
                                                                    edge))]
    [(conj nodes {:type :constant :value (pure-rand-nth value-seed [:true :false])})
     (if (and index (integer? index))
       (conj (assoc (into [] edges)
                    index
                    (dissoc edge stolen-sink-index))
             {(count nodes) :output stolen-sink-index stolen-sink-port})
       (conj edges {(count nodes) :output}))]))

(defmacro MUTATIONS [] [change-latch-trigger
                        change-names-remove-clause
                        change-names-add-clause
                        change-constant-value
                        change-latch-initial])

(defn random-mutation [] (rand-nth [`change-latch-trigger
                                    `change-names-remove-clause
                                    `change-names-add-clause
                                    `change-constant-value
                                    `change-latch-initial
                                    `change-names-remove-clause
                                    `change-names-add-clause
                                    `change-names-flip-term
                                    `add-output
                                    `add-input
                                    `add-constant
                                    `edge-switch-source
                                    `edge-steal-sink]))

(defmacro MUTATIONS-BY-TYPE [] '{:latch [change-latch-trigger
                                         change-latch-initial]
                                 :names [change-names-remove-clause
                                         change-names-add-clause]
                                 :constant [change-constant-value]})

(defn mutate [g] ((rand-nth (MUTATIONS)) g))
