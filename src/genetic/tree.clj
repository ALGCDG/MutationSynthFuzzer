(ns genetic.tree
  (:use genetic.mutation)
  (:require [genetic.representation :refer [genetic-representation]])
  (:require [genetic.crossover :refer [dumb-crossover lossless-crossover compatible-crossover check-compatible]]))

;; Represent generated tests as S expression of mutations and crossovers of original corpus.
;; Allows us to create family tree of tests
;; Potnetially useful for debugging evolution
;; NB: useless until seeded randomness is used

(defn add-mutation [g] `(~(random-mutation) ~(rand-int Integer/MAX_VALUE) ~g))

(defn add-crossover [[a b]] (if (check-compatible (eval a) (eval b))
                              `(compatible-crossover ~(rand-int Integer/MAX_VALUE) ~a ~b)
                              `(~(rand-nth [`dumb-crossover
                                            `lossless-crossover])
                                ~(rand-int Integer/MAX_VALUE) ~a ~b)))


(def tree-eval (memoize eval))  ;; recognise already evaluated branches

;;(add-mutation '(genetic-representation "example.blif.old"))
;;
;;(println (add-mutation `(genetic-representation "example.blif.old")))
;;
;;(eval (add-mutation '(genetic-representation "example.blif.old")))
;;
;;(add-crossover ['(genetic-representation "example.blif.old") '(genetic-representation "example.blif.old")])
;;
;;(add-crossover ['(genetic-representation "example.blif.old") (add-mutation '(genetic-representation "example.blif.old"))])
;;
;;(println (add-crossover [`(genetic-representation "example.blif.old") `(genetic-representation ~"example.blif.old")]))
;;
;;(tree-eval (add-crossover ['(genetic-representation "example.blif.old") '(genetic-representation "example.blif.old")]))
;;
;;(-> (iterate add-mutation '(genetic-representation "example.blif.old")) (nth 5))
;;
;;(spit "state.clj" (-> (iterate add-mutation '(genetic-representation "example.blif.old")) (nth 5)))
;;;; Write genetic state to file
