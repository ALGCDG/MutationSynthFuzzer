(ns tester
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:use blif.parser)
  (:use blif.compose)
  (:use genetic.representation)
  (:use genetic.mutation)
  (:use genetic.crossover))

(pp/pprint (parse (slurp "example.blif")))

(print "\n--------\n")

(pp/pprint (genetic-representation "example.blif"))
(print "\n--------\n")

(pp/pprint (let [[nodes edges] (genetic-representation "example.blif")]
             (map-indexed (fn [index node] (generate node index edges)) nodes)))

(map print (str/join "\n" (let [[nodes edges] (genetic-representation "example.blif")]
                            (map-indexed (fn [index node] (generate node index edges)) nodes))))

(pp/pprint (fix-edge {6 1 7 2 8 3} [] {6 :input, 7 :output, 8 :input0}))
(pp/pprint (fix-edge {6 1} [11 12 13 14] {6 :input, 7 :output, 8 :input0}))

(print "\n--------\n")
(pp/pprint (dumb-crossover (genetic-representation "example.blif") (genetic-representation "example.blif")))

(print (generate-blif (dumb-crossover (genetic-representation "example.blif") (genetic-representation "example.blif"))))

(print "\n--------\n")
(pp/pprint (change-latch-trigger (genetic-representation "example.blif")))

(print "\n--------\n")
(pp/pprint (change-constant-value (genetic-representation "example.blif")))

(print "\n--------\n")
(pp/pprint (let [[n e] (genetic-representation "example.blif")] (update-edges 10 e)))

(print "\n--------\n")
(pp/pprint (change-names-add-clause (genetic-representation "example.blif")))
(print "\n--------\n")
(pp/pprint (change-names-remove-clause (genetic-representation "example.blif")))

(pp/pprint (mutate (genetic-representation "example.blif")))

(print (generate-blif (-> (iterate mutate (genetic-representation "example.blif")) (nth 1000))))
