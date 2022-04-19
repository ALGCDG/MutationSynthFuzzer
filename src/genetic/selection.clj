(ns genetic.selection
  (:require [clojure.data.json :as json])
  (:require [genetic.crossover :refer [crossover]])
  (:require [genetic.mutation :refer [mutate]]))

(defmacro NUM-SURVIVORS [] 10)
(defmacro MUTATION-PROBABILITY [] 0.5)
(defmacro CROSSOVER-PROBABILITY [] 0.2)

(defn next-population [population]
  (concat (->> population
               (random-sample (MUTATION-PROBABILITY))
               (map mutate))
          (->> (apply map list population population)
               (random-sample (CROSSOVER-PROBABILITY))
               (map crossover))))

(defn calculate-coverage [coverage-json] 0)  ;; PLACEHOLDER

;;(defn run-synthesis [g]
;;  (let [generated-filename (format "%s.blif" (hash g))]
;;    (sh "rm" "*.gcda")  ;; CLEAN COVERAGE DATA (DELETE EXISTING FILES) TODO FIX 
;;    (spit generated-filename (generate-blif g))
;;    (spit generated-filename (generate-blif g))
;;    (sh "bash" "-c" (format  "timeout %s %s %s %s"
;;                             (TIMEOUT)
;;                             (SYNTH-PATH)
;;                             (SYNTH-ARGS)
;;                             generated-filename))
;;    (sh "bash" "-c" (format  "gcov --json-format -t %s" (COVERAGE-FILE)))  ;; Convert coverage file to JSON and read 
;;    (calculate-coverage (json/read-str (slurp "coverage.txt")))))
;;
;;(defn get-coverage [] 0)  ;; PLACEHOLDER
;;
;;(defn select [genes coverage]
;;  (take (NUM-SURVIVORS) (sort-by get-coverage genes)))
