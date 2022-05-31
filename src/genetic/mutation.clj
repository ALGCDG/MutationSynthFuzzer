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
          [(assoc (into [] nodes) modified-index new-node) edges])))))

(defn change-latch-initial [seed [nodes edges]]
  (let [[node-seed initial-seed] (generate-seeds seed 2)]
    (let [[modified-index node] (rand-nodetype node-seed nodes :latch)]
      (let [other-initial (set/difference (set (range 4)) #{(get node :initial)})]
        (let [new-node (assoc node :initial (pure-rand-nth initial-seed (into [] other-initial)))]
          [(assoc (into [] nodes) modified-index new-node) edges])))))

(defn change-constant-value [seed [nodes edges]]
  (let [[modified-index node] (rand-nodetype seed nodes :constant)]
    (let [new-node (assoc node :value (if (= (get node :value) :true) :false :true))]
      [(assoc (into [] nodes) modified-index new-node) edges])))

(defn change-names-flip-term [seed [nodes edges]]
  (let [[node-seed row-seed symbol-seed char-seed] (generate-seeds seed 4)
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
                                (pure-rand-nth char-seed (set/difference (set (CNF-SYMBOLS)) #{target-symbol}))))
        new-node (assoc node :table new-table)]
    [(assoc (into [] nodes) modified-index new-node) edges]))

(defn change-names-remove-clause [seed [nodes edges]]
  (let [[modified-index node] (rand-nodetype seed nodes :names)]
    (let [original-table (get node :table)]
      (if (not (> 0 (count original-table))) (throw (ex-info ".names must have at least one row to avoid unknowns." {})))
      (let [new-table (rest (shuffle original-table))]
        (let [new-node (assoc node :table new-table)]
          [(assoc (into [] nodes) modified-index new-node) edges])))))

(defn change-names-add-clause [seed [nodes edges]]
  (let [[node-seed sample-seed] (generate-seeds seed 2)]
    (let [[modified-index node] (rand-nodetype node-seed nodes :names)]
      (let [original-table (get node :table)]
        (let [n (+ 1 (get node :num-inputs))]
          (let [new-clause (take n (pure-sample sample-seed 0.1 (cycle (CNF-SYMBOLS))))]  ;; Note that the probability 0.1 is arbitrary, we are performing a uniform sample (just need to make sure probability is not 1).
            (let [new-table (conj original-table [new-clause])]
              (let [new-node (assoc node :table new-table)]
                [(assoc (into [] nodes) modified-index new-node) edges]))))))))

(defn cut [index col]
  (concat (take index col) (take-last (- (count col) index 1) col)))

(defn change-names-remove-input [seed [nodes edges]]
  (let [[node-seed input-seed] (generate-seeds seed 2)
        [target-index target-node] (rand-nodetype node-seed nodes :names)
        target-input (->> target-node :num-inputs range (pure-rand-nth input-seed))]
    [(assoc (into [] nodes)
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
    [(assoc (into [] nodes)
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
    (if (edge-a source-index-b) (throw (ex-info (format "Could not move %d:%s because receiving edge already contains %d:%s, possible loop"
                                                 source-index-b
                                                 :output
                                                 source-index-b
                                                 (edge-a source-index-b))
                                                {})))
    (if (edge-b source-index-a) (throw (ex-info (format "Could not move %d:%s because receiving edge already contains %d:%s, possible loop"
                                                  source-index-a
                                                  :output
                                                  source-index-a
                                                  (edge-b source-index-a))
                                                {})))
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
    (if (not (or (not (theif-edge stolen-sink-index))
                (not= (theif-edge stolen-sink-index) :output)))
            (throw (ex-info (format "Loop detected when stealing %d:%s (conflicts with existing output from node %d)."
                    stolen-sink-index
                    stolen-sink-port
                    stolen-sink-index)
                            {})))
    (if (theif-edge stolen-sink-index)
            (throw (ex-info (format "Stealing %d:%s would overwrite existing %d:%s in theif edge."
                    stolen-sink-index
                    stolen-sink-port
                    stolen-sink-index
                    (theif-edge stolen-sink-index))
                            {})))
    [nodes
     (assoc (into [] edges)
            theif-index
            (assoc theif-edge stolen-sink-index stolen-sink-port)
            target-index
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

(defn add-latch [seed [nodes edges]]
  (let [[edge-seed sink-seed trigger-seed initial-seed clk-src-seed input-seed] (generate-seeds seed 6)
        [clk-index clk-edge] (pure-rand-nth clk-src-seed (enumerate edges))
        [input-index input-edge] (pure-rand-nth input-seed (enumerate edges))
        [target-index target-edge] (pure-rand-nth edge-seed
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
    [(conj nodes {:type :latch
                  :trigger (pure-rand-nth trigger-seed #{"re" "fe" "ah" "al" "as"})
                  :initial (->> (range 4) (pure-rand-nth initial-seed) str)})
     (conj (assoc (into [] edges)
                  clk-index
                  (assoc clk-edge (count nodes) :clk)
                  input-index
                  (assoc input-edge (count nodes) :input)
                  target-index
                  (dissoc target-edge stolen-sink-index))
           {(count nodes) :output stolen-sink-index stolen-sink-port})]))

(defn add-names [seed [nodes edges]]
  (let [[edge-seed sink-seed num-inputs-seed symbol-seed clause-seed input-seed] (generate-seeds seed 6)
        num-inputs (->> edges count (range 1) (pure-rand-nth num-inputs-seed))
        new-sinks (for [[in-seed input-index] (map vector (generate-seeds input-seed num-inputs) (range num-inputs))]
                (let [[i e] (pure-rand-nth in-seed (enumerate edges))]
                  [i (assoc e (count nodes) (input-keyword input-index))]))
        [target-index target-edge] (pure-rand-nth edge-seed
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
    [(conj nodes {:type :names
                  :num-inputs num-inputs
                  :table (let [num-clause (pure-rand-nth clause-seed (range 1 1000))]
                           (->> (CNF-SYMBOLS)
                                cycle
                                (pure-sample symbol-seed 0.1)
                                (take (* num-clause (+ 1 num-inputs)))
                                (partition (+ 1 num-inputs))))})
     (conj (apply (partial assoc (into [] edges)) (flatten new-sinks))
           {(count nodes) :output stolen-sink-index stolen-sink-port})]))

(defn remove-node [seed [nodes edges]]
  (let [[node-seed surrogate-seed] (generate-seeds seed 2)
        [missing-index n] (pure-rand-nth node-seed (enumerate nodes))
        cleaned-edges (mapv #(dissoc % missing-index) edges)
        missing-output-edges (filter #(->> % set/map-invert :output not) cleaned-edges)
        loose-sinks (flatten (mapv #(into [] %) missing-output-edges))
        suitable-edges (filter #(->> % set/map-invert :output) cleaned-edges)
        [surrogate-index surrogate-edge] (pure-rand-nth surrogate-seed (enumerate suitable-edges))]
    [(assoc (into [] nodes) missing-index nil)
     (if (not (empty? loose-sinks))
       (assoc (into [] suitable-edges)
              surrogate-index
              (apply (partial assoc surrogate-edge) loose-sinks))
       suitable-edges)]))

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
                                    `add-names
                                    `add-latch
                                    `remove-node
                                    `edge-switch-source
                                    `edge-steal-sink]))

(defmacro MUTATIONS-BY-TYPE [] '{:latch [change-latch-trigger
                                         change-latch-initial]
                                 :names [change-names-remove-clause
                                         change-names-add-clause]
                                 :constant [change-constant-value]})

(defn mutate [g] ((rand-nth (MUTATIONS)) g))
