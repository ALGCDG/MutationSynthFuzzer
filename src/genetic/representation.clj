(ns genetic.representation
  (:require [clojure.spec.alpha :as s])
  (:require [blif.parser :refer [parse get-modules]]))

(defn get-vars [ports]
  (let [vars (map keys ports)] (apply concat vars)))
;; to find edge genes, iterate through variables, find edges where they connect

(defn discover-edge [ports var]
  {:post [(s/assert :node/edge %)]}  ;; Post condition, any edge has only one output
  (->> (map-indexed (fn [index port] (if (contains? port var) {index (get port var)})) ports)
       (filter identity)
       (apply merge)))

(defn ports-to-edges [ports]
  (map (partial discover-edge ports) (get-vars ports)))

(defn genetic-representation [f]
  (let [[nodes ports] (parse (first (get-modules (slurp f))))]
    (let [edges (ports-to-edges ports)]
      [nodes edges])))

(s/def ::keyword #{:names :input :outputs :latch :constant})
(s/def :node/type ::keyword)
(s/def :node/common (s/keys :req [:node/type]))
(s/def :node/latch-trigger-type #{"re" "fe" "ah" "al" "as"})
(s/def :node/latch-initial string?)
(s/def :node/latch (s/merge :node/common (s/keys :req [:node/latch-trigger-type :node/latch-initial])))
(s/def :node/names-num-inputs (s/and int? pos?))
;;(s/def :node/names/table )
;;(s/def :node/names (s/merge :node/common (s/keys :req [:node/names/num-inputs :node/names/table])))
(s/def :node/constant-value #{:true :false})
(s/def :node/constant (s/merge :node/common (s/keys :req [:node/constant-value])))
(s/explain :node/common {:node/type :latch})
;; TODO perhaps investigate using multi=spec

(s/def ::one-output #(= 1 (count (filter (partial = :output) (vals %)))))
(s/def ::standard-port #{:output :input :clk})
(s/def ::indexed-port #(re-matches #":input\d+" (str %)))
(s/def ::port-keyword (s/or ::standard-port ::indexed-port))
(s/def :node/edge (s/and (s/map-of int? keyword?) ::one-output))

(s/def :node/edges (s/* :node/edge))
(s/def :node/nodes (s/* :node/common))
(s/def :node/in (s/tuple :node/nodes :node/edges))
