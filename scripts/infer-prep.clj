#! /usr/bin/env clojure

(require '[clojure.string :as str])
(require '[clojure.data.json :as json])

(let [bugs (->> *command-line-args* first slurp json/read-str)
      files (->> bugs (map #(% "file")) set)]
  (spit "infer.clj"
        (apply merge
               (for [file files]
                 {(last (str/split file #"\/")) (->> bugs
                            (filter #(= (% "file") file))
                            (map #(% "line")))}))))
