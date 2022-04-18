(ns blif.parser
  (:require [clojure.string :as str])
  (:require [util :refer [input-keyword]]))

(defmacro LINEEND [] #"\n+")
(defmacro KEYWORD [] #"(?=(\.names|\.subckt|\.inputs|\.outputs|\.latch|\.end))")

(defmacro unique-id [] "0")

(defn get-modules [s] (filter (fn [x] (str/includes? x ".model")) (str/split s #"(?=.model)")))
(defn blocks [s] (filter (fn [x] (re-find (KEYWORD) x)) (str/split s (KEYWORD))))

(defn check-unimplemented [s]
  (let [lines (map str/trim (str/split s (LINEEND)))]
    (let [keywords (filter identity (map (partial re-matches #"^\.\S+") lines))]
      (let [unrecognised (filter #(not (re-find (KEYWORD) %)) keywords)]
        (if (not (empty? unrecognised))
          (binding [*out* *err*]
            (println (format "Unrecognised Keywords: %s" (str/join " " unrecognised)))))))))

(defn manage-input-node [name] {:type :input})
(defn manage-input-port [name] {name :output})
(defn manage-inputs [block [nodes edges]]
  (let [args (rest (str/split (str/trim block) #"(\s|\n)+"))]
    (let [new-nodes (map manage-input-node args)]
      (let [new-ports (map manage-input-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-output-node [name] {:type :output})
(defn manage-output-port [name] {name :input})
(defn manage-outputs [block [nodes edges]]
  (let [args (rest (str/split (str/trim block) #"(\s|\n)+"))]
    (let [new-nodes (map manage-output-node args)]
      (let [new-ports (map manage-output-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-names [block [nodes edges]]
  (let [lines (str/split block (LINEEND))]
    (let [table-rows (rest lines)
          args (rest (str/split (str/trim (first lines)) #"\s+"))]
      (let [output (last args)]
        (case table-rows
          [] [(conj nodes {:type :constant :value :false}) (conj edges {output :output})]
          ["1"] [(conj nodes {:type :constant :value :true}) (conj edges {output :output})]
          (let [inputs (butlast args)
                cnf (map #(str/split % #"\s+") table-rows)]
            (let [input-map (->> (map-indexed (fn [index variable] {variable (input-keyword index)}) inputs)
                                 (apply merge))]
              [(conj nodes {:type :names :num-inputs (count inputs) :table cnf})
               (conj edges (merge input-map {output :output}))])))))))

(defn manage-latch [block [nodes edges]]
  (let [args (rest (str/split (str/trim block) #"(\s|\n)+"))]
    (let [[input output type clock initial] args]
      [(conj nodes {:type :latch :trigger-type type :initial (or initial "3")})
       (conj edges {input :input output :output clock :clk})])))

(defn manage-subckt [block [nodes edges]]
  (println "Uhoh SUBCKT") [nodes edges])
;;  (let [args (drop 1 (str/split (str/trim block) #"\s+"))]
;;    (let [model-name (first args)]
;;      (let [port-connections (rest args)]
;;        (let [node-name (str model-name (unique-id))]
;;          (let [new-edges (map (fn [x] (let [[port source] (str/split (str/trim x) #"=")] {source (str node-name "." port)})) port-connections)]
;;            [(concat nodes [node-name]) (concat edges new-edges)]))))))

(defn manage [block [nodes edges]]
  (let [keyword (first (str/split (str/trim block) #"\s+"))]
    (case keyword
      ".inputs" (manage-inputs block [nodes edges])
      ".outputs" (manage-outputs block [nodes edges])
      ".names" (manage-names block [nodes edges])
      ;;".subckt" (manage-subckt block [nodes edges])
      ".latch" (manage-latch block [nodes edges])
      [nodes edges])))

(defn parse [s]
  (check-unimplemented s)
  (loop [blcks (blocks s) [nodes edges] [[] []]]
    (case blcks
      '() [nodes edges]
      nil [nodes edges]
      (let [[head & tail] blcks]
        (recur tail (manage head [nodes edges]))))))
