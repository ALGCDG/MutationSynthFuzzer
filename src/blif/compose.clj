(ns blif.compose
  (:require [clojure.string :as str])
  (:require [util :refer [input-keyword log]]))

(defmacro MISSING-EDGE [] "missing_edge")

(defmacro GENERATED-MODULE-NAME [] "presynth")

(defmacro MODULE-CLK [] "clk")

(defn edge-to-var [edge]
  {:pre (< (count edge) 3)
   :post (string? %)}
  (if (edge :missing) (MISSING-EDGE)
  (format "wire%s" (-> (keys edge)
                    #_(or ,,, (MISSING-EDGE))
                    pr-str
                    (str/replace ,,, #"\s" "_")
                    (str/replace ,,, #"\(|\)" "")))))

(defn create-var [index port edges]
  (let [check
        (fn [edge]
          (and
           (.contains (keys edge) index)
           (= (get edge index) port)))]
    (let [edge (filter check edges)]
      (if (first edge)
        (edge-to-var (first edge))
        (do (log (format "Failed to create variable for index %d and port %s" index port))
            (MISSING-EDGE))))))

(defn generate-input [node index edges]
  (format ".inputs %s" (create-var index :output edges)))

(defn generate-output [node index edges]
  (format ".outputs %s" (create-var index :input edges)))

(defn generate-latch [config node index edges]
  (let [input (create-var index :input edges)
        output (create-var index :output edges)
        clk (create-var index :clk edges)
        trigger (get node :trigger-type)
        initial (get node :initial)]
    (format ".latch %s %s %s %s %s"
            input
            output
            trigger
            (if (config :force-clk) (MODULE-CLK) clk)
            initial)))

(defn generate-constant [node index edges]
  (let [table (if (= :true (get node :value)) "\n1" "")]
    (format ".names %s %s" (create-var index :output edges) table)))

(defn generate-names [node index edges]
  (let [input-labels (map input-keyword (range (get node :num-inputs)))]
    (let [args (map #(create-var index % edges) input-labels)]
      (let [output (create-var index :output edges)]
        (format ".names %s %s \n%s"
                (str/join " " args)
                (create-var index :output edges)
                (str/join "\n" (map #(format "%s %s"
                                             (str/join "" (butlast %))
                                             (last %)) (get node :table))))))))

(defn generate [config node index edges]
  (case (get node :type)
    :input (generate-input node index edges)
    :output (generate-output node index edges)
    :latch (generate-latch config node index edges)
    :constant (generate-constant node index edges)
    :names (generate-names node index edges)
    (throw "Unrecognised Node Type encountered during BLIF generation.")))

(defn generate-blif [config [nodes edges]]
  (if (not (empty? (filter #(->> % vals (filter (partial = :output)) count (= 1) not) edges)))
    (throw (ex-info "Edge does not have single output port" {:type :invalid-state})))
  (format ".model %s\n%s\n.names %s\n%s\n.end"
          (GENERATED-MODULE-NAME)
          (str/join "\n" (map-indexed (fn [index node] (generate config node index edges)) nodes))
          (str (MISSING-EDGE))
          (if (config :force-clk) (format ".inputs %s" (MODULE-CLK)) "")))
