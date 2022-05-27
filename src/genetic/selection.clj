(ns genetic.selection
  (:require [clojure.data.json :as json])
  (:require [clojure.math.combinatorics :refer [cartesian-product]])
  (:require [genetic.crossover :refer [crossover]])
  (:require [genetic.tree :refer [add-mutation add-crossover]]))

;; Default selection parameters
(defmacro NUM-SURVIVORS [] 10)
(defmacro MUTATION-PROBABILITY [] 0.5)
(defmacro CROSSOVER-PROBABILITY [] 0.05)

(defn next-population [config population]
  (let [survivors (take (or (config :num-survivors) (NUM-SURVIVORS)) (sort-by #(->> % first -) population))]
    {:tested survivors
     :untested (concat (->> survivors
                            (map second)
                            (random-sample (or (config :mutation-probability)
                                               (MUTATION-PROBABILITY)))
                            (map add-mutation))
                       (->> survivors
                            (map second)
                            (#(cartesian-product % %))
                            (random-sample (/ (or (config :crossover-probability)
                                                  (CROSSOVER-PROBABILITY))
                                              (count survivors)))  ;; Normalising by number of survivors to counter-act cartesian product
                            (map add-crossover)))}))
