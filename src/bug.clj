(ns bug
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:use genetic.mutation)
  (:use genetic.crossover)
  (:use genetic.representation)
  (:require [blif.compose :refer [generate-blif]])
  (:require [equivalence :refer [simulate]])
  #_(:require [core :refer [run-test]]))

(defn recreate-blif [bug]
  (->> bug :input eval generate-blif))

(->> (slurp "EF560692.bug.log") println)

(->> (slurp "EF560692.bug.log") read-string recreate-blif println)

#_(defn rerun [synth synth-path yosys-path sby-path abc-path bug]
    (try
      (->> bug
           :input
           (fn [x] (run-test synth synth-path yosys-path sby-path abc-path x ".")))
      (catch Exception e e)))

(->> (slurp "bugs/FE0488F3.bug.log") read-string :error :pre-synth-verilog (spit "bug_pre.v"))
(->> (slurp "bugs/FE0488F3.bug.log") read-string :error :post-synth-verilog (spit "bug_post.v"))

(->> (slurp "bugs/FE0488F3.bug.log") read-string :error :proof :out (spit "bug_proof.txt"))

;;(->> (slurp "bugs/FE0488F3.bug.log") read-string :input (#(clojure.pprint/write % :dispatch clojure.pprint/code-dispatch)))

(defn tree-bug [bug]
  (-> bug :input (pp/write ,,, :dispatch pp/code-dispatch)))

(->> (slurp "bugs/FE0488F3.bug.log") read-string tree-bug)

#_(->> (slurp "bugs/FE0488F3.bug.log") read-string (rerun :yosys "../yosys/yosys" "/Users/archie/yosys/yosys" "../Smbiyosys/sbysrc/sby.py" "/Users/archie/yosys/yosys-abc") println)

;; USEFUL COMMANDS
;; get counter example (for equiv-failures) 
;; simulate counter example (for equiv-failures) 

(->> "bugs"
     clojure.java.io/file
     file-seq
     (shuffle)
     (take 50)
     (map str)
     (filter #(str/includes? % ".bug.log"))
     (map slurp)
     (map read-string)
     (map :error)
     (map :type)
     (filter #(= % :synth-fail))
     #_(filter #(->> % :error :type (= :synth-fail))))

(->> "bugs"
     clojure.java.io/file
     file-seq
     (shuffle)
     (take 20)
     (map str)
     (filter #(str/includes? % ".bug.log"))
     (map slurp)
     (map read-string)
     (filter #(->> % :error :type (= :synth-fail))))

(->> "bugs"
     clojure.java.io/file
     file-seq
     (shuffle)
     (take 20)
     (map str)
     (filter #(str/includes? % ".bug.log"))
     (map slurp)
     (map read-string)
     (filter #(->> % :error :type (= :synth-fail)))
     count
     println)

(defn tryread [file]
  (try
    (->> file slurp read-string)
    (catch Exception e
      (println (format "WARNING: Could not read bug report: %s" file))
      nil)))

(defn read-bugs [dir]
  (let [bug-files (->> dir
                       clojure.java.io/file
                       file-seq
                       (map str)
                       (filter #(str/includes? % ".bug.log")))
        bugs (filter identity (map tryread bug-files))]
    bugs))

(defn is-synth-fail [bug] (->> bug :error :type (= :synth-fail)))
(defn is-equiv-fail [bug] (->> bug :error :type (= :equiv-fail)))
(defn is-timeout [bug] (and (->> bug :error :type (= :synth-fail))
                            (->> bug :error :result :exit (= 124))
                            (->> bug :error :result :err empty?)))
(defn classify-bugs [bugs]
  {:synth-fail (filter #(->> % :error :type (= :synth-fail)) bugs)
   :equiv-fail (filter #(->> % :error :type (= :equiv-fail)) bugs)})

(defn -main []
  (let [grouped-bugs (classify-bugs (read-bugs "bugs"))]
    (println (format "%s Synthesis bugs" (->> grouped-bugs :synth-fail count)))
    (println (format "%s Equivalence bugs" (->> grouped-bugs :equiv-fail count)))))

(->> "bugs" read-bugs classify-bugs :synth-fail (filter #(->> % :error :result :err (not= ""))))

#_(->> "bugs"
       clojure.java.io/file
       file-seq
       (filter #(str/includes? % ".bug.log"))
       (map str)
       (map slurp)
       (map read-string)
       (filter #(->> % :type (= :synth-error))))

#_(defn simulate [syb-path yosys-path abc-path smtbmc-path python-path g tmpfile pre-synth-path post-synth-path]
    (let [top-path (format "%s/top.v" tmpfile)]
      (spit top-path (top g))
      (let [proof-result (run-sby syb-path yosys-path abc-path python-path tmpfile top-path pre-synth-path post-synth-path smtbmc-path)]
        (sh (format "iverilog -g2012 %s %s %s %s -o sim"
                    top-path
                    pre-synth-path
                    post-synth-path
                    counter-eg-tb-path))
        (sh "bash" "-c" "vvp sim"))))

(defn simulate-bug [config bug]
  (spit "sut_post.v" (->> bug :error :post-synth-verilog))
  (spit "sut_pre.v" (->> bug :error :pre-synth-verilog))
  (simulate config (->> bug :input eval) "." "sut_pre.v" "sut_post.v"))

(defn -main []
  (let [grouped-bugs (classify-bugs (read-bugs "bugs"))]
    (println (format "%s Synthesis bugs" (->> grouped-bugs :synth-fail count)))
    (println (format "%s Equivalence bugs" (->> grouped-bugs :equiv-fail count))))
  (let [simulation-config (merge {:smtbmc-path "/Users/archie/yosys/yosys-smtbmc"}
                                 (->> "config.clj" slurp read-string))]
    (->> "bugs"
       read-bugs
       classify-bugs
       :equiv-fail
       (mapv (fn [bug]
               [bug
                (simulate-bug simulation-config bug)]))
       (filter (fn [[bug result]] (->> result :out empty? not)))
       println)))

