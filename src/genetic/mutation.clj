(ns genetic.mutation
  (:require [clojure.set :as set])
  (:require [util :refer [enumerate]])
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

;;(defn change-names-flip-term [seed [nodes edges]]
;;  (let [indexed-names-nodes (filter (fn [[index node]] (= (get node :type) :names)) (map-indexed vector nodes))]
;;    (let [[modified-index node] (pure-rand-nth indexed-names-nodes)]
;;      (let [original-table (get node :table)]
;;        (let [new-table (rest (shuffle original-table))]
;;          (let [new-node (assoc node :table new-table)]
;;            [(assoc nodes modified-index new-node) edges]))))))

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

(defmacro MUTATIONS [] [change-latch-trigger
                        change-names-remove-clause
                        change-names-add-clause
                        change-constant-value
                        change-latch-initial])

(defn random-mutation [] (rand-nth [`change-latch-trigger
                                    `change-names-remove-clause
                                    `change-names-add-clause
                                    `change-constant-value
                                    `change-latch-initial]))

(defmacro MUTATIONS-BY-TYPE [] '{:latch [change-latch-trigger
                                         change-latch-initial]
                                 :names [change-names-remove-clause
                                         change-names-add-clause]
                                 :constant [change-constant-value]})

(defn mutate [g] ((rand-nth (MUTATIONS)) g))
