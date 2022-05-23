(ns graph
  (:require [clojure.string :as str]))

(defn graph [tree]
  (if (or (symbol? tree)
          (< (count tree) 3))
    "terminal"
    (let [op (first tree)
          seed (second tree)]
      (case (count tree)
        3 [(format "mutate %d" seed) (graph (last tree))]
        4 [(format "crossover %d" seed) (nth tree 2) (nth tree 3)]
        "terminal"))))

(graph '(f 0 (g 0 (h 0 x y))))

(defn dot [tree]
  (if (or (symbol? tree)
          (< (count tree) 3))
    "terminal"
    (let [op (first tree)
          seed (second tree)]
      (case (count tree)
        3 (str/join " -> " [(format "mutate %d" seed) (dot (last tree))])
        4 (format "crossover %d \n crossover %d -> %s \n crossover %d -> %s " seed seed (dot (nth tree 2)) seed (dot (nth tree 3)))
        "terminal"))))

(dot '(f 0 (g 0 (h 0 x))))

(dot '(f 0 (g 0 (h 0 (i 0 (j 0 x)) y))))

(defn dote [tree prefix depth]
  (let [node-id (format "%s%d" prefix depth)]
    (if (or (symbol? tree)
            (< (count tree) 3))
      node-id
      (let [op (first tree)
            seed (second tree)]
        (case (count tree)
          3 (format "%s -> %s"
                    node-id
                    (dote (last tree) prefix (+ 1 depth)))
          4 (format "%s\n%s -> %s\n%s -> %s"
                    node-id
                    node-id
                    (dote (nth tree 2) (format "%sl" prefix) depth)
                    node-id
                    (dote (nth tree 3) (format "%sr" prefix) depth))
          node-id)))))

(defn dotn [tree prefix depth]
  (let [node-id (format "%s%d" prefix depth)]
    (if (symbol? tree)
      {node-id {"label" "terminal"}}
      (let [op (first tree)
            seed (second tree)]
        (case (count tree)
          3 (merge
             {node-id {"label" (format "mutation\\n%s\\n%d" op seed)
                       "shape" "record"
                       "fillcolor" "green"
                       "style" "rounded, filled"}}
             (dotn (last tree) prefix (+ 1 depth)))
          4 (merge
             {node-id {"label" (format "crossover\\n%s\\n%d" op seed)
                       "shape" "diamond"
                       "fillcolor" "yellow"
                       "style" "rounded, filled"}}
             (dotn (nth tree 2) (format "%sl" prefix) depth)
             (dotn (nth tree 3) (format "%sr" prefix) depth))
          {node-id {"label" (format "terminal\\n%s" (str/join "\\n" tree))}})))))

;;(dotn '(f 0 (g 0 (h 0 x))) "t" 0 {})
;;
;;(dotn '(f 0 (g 0 (h 0 (i 0 (j 0 x)) y)))  "t" 0)

(defn dott [tree]
  (format "digraph G {\n%s\n%s\n}"
          (str/join "\n" (for [[node-id args] (dotn tree "t" 0)]
                           (format "%s [%s]"
                                   node-id
                                   (str/join ", " (for [[a b] args] (format "%s=\"%s\"" a b))))))
          (dote tree "t" 0)))

(println (dott '(f 1 (g 2 (h 3 (i 4 (j 5 x)) y)))))

(->> (slurp "example_tree.clj") read-string dott println)
