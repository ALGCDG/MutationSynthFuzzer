(ns genetic.mutation
  (:require [clojure.set :as set])
  (:require [util :refer [enumerate]]))

(defmacro CNF-SYMBOLS [] ["0" "1" "-"])

(defn find-nodes-indexed [nodes type]
  (filter (fn [[index node]] (= (get node :type) type)) (enumerate nodes)))

(defn rand-nodetype [nodes type] (rand-nth (find-nodes-indexed nodes type)))

(defn change-latch-trigger [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :latch)]
    (let [other-triggers (set/difference #{:re, :fe, :as, :ah, :al} #{(get node :trigger-type)})]
      (let [new-node (assoc node :trigger-type (rand-nth (into [] other-triggers)))]
        [(assoc nodes modified-index new-node) edges]))))

(defn change-latch-initial [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :latch)]
    (let [other-initial (set/difference (set (range 4)) #{(get node :initial)})]
      (let [new-node (assoc node :initial (rand-nth (into [] other-initial)))]
        [(assoc nodes modified-index new-node) edges]))))

(defn change-constant-value [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :constant)]
    (let [new-node (assoc node :value (if (= (get node :value) :true) :false :true))]
      [(assoc nodes modified-index new-node) edges])))

;;(defn change-names-flip-term [[nodes edges]]
;;  (let [indexed-names-nodes (filter (fn [[index node]] (= (get node :type) :names)) (map-indexed vector nodes))]
;;    (let [[modified-index node] (rand-nth indexed-names-nodes)]
;;      (let [original-table (get node :table)]
;;        (let [new-table (rest (shuffle original-table))]
;;          (let [new-node (assoc node :table new-table)]
;;            [(assoc nodes modified-index new-node) edges]))))))

(defn change-names-remove-clause [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :names)]
    (let [original-table (get node :table)]
      (let [new-table (rest (shuffle original-table))]
        (let [new-node (assoc node :table new-table)]
          [(assoc nodes modified-index new-node) edges])))))

(defn change-names-add-clause [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :names)]
    (let [original-table (get node :table)]
      (let [n (+ 1 (get node :num-inputs))]
        (let [new-clause (take n (random-sample 0.1 (cycle (CNF-SYMBOLS))))]  ;; Note that the probability 0.1 is arbitrary, we are performing a uniform sample (just need to make sure probability is not 1).
          (let [new-table (conj original-table [new-clause])]
            (let [new-node (assoc node :table new-table)]
              [(assoc nodes modified-index new-node) edges])))))))

(defmacro MUTATIONS [] '[change-latch-trigger
                         change-names-remove-clause
                         change-names-add-clause
                         change-constant-value
                         change-latch-initial])

(defn mutate [g] ((rand-nth (MUTATIONS)) g))
