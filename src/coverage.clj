(ns coverage
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]])
  (:require [clojure.java.io :refer [delete-file]])
  (:require [util :refer [log]]))

;; Fuzzer assumes that provided syntheiszer is instrumented with gcov.

(defn convert-to-json [gcda-file]
  (sh "bash" "-c" (format "gcov --json-format -t %s" gcda-file)))  ;; Is there a OSX Linux agnostic way of doing this?

(defn dependencies [src-dir target]
  (->>
   (sh "bash" "-c" (format "make -Bn %s" target) :dir src-dir)
   :out
   (re-seq #"\S*\.cc?")
   #_(filter #(.exists (clojure.java.io/file %)))))

;;(first (dependencies "/Users/archie/yosys" "yosys"))

(defn find-src-files [src-dir]
  (->> src-dir
       clojure.java.io/file
       file-seq
       (map str)
       (filter #(re-matches #"\S+\.cc?" %))))

;;(first (find-src-files "/Users/archie/yosys"))

(defn run-gcov [files tmpfile]
  (sh "bash" "-c" (format "gcov %s" (str/join " " files)) :dir tmpfile))

;;(re-seq #"\S*\.o" "hello.o word world.o")

(defn gcov-line [s]
  (let [[execution-count line-number & other] (map str/trim (str/split s #":"))]
    (if (and line-number (re-matches #"\d+" line-number))
      (if (and execution-count (re-matches #"\d+" execution-count))
        {(Long. line-number) (Long. execution-count)}
        {(Long. line-number) 0}))))

;;(gcov-line "12:1: a;sdlkjfas")
;;
;;(gcov-line "####:1: a;sdlkjfas")

(defn gcov-file [s]
  (->> (str/split s #"\n") (map gcov-line) (filter identity) (apply merge)))

;;(->> (slurp "../yosys/yosys.cc.gcov") gcov-file)

;;(gcov-file (slurp "../yosys/yosys.cc.gcov"))

(defn bellcurve [x]
  (* (/ 1 (java.lang.Math/sqrt (* (java.lang.Math/PI) 2))) (java.lang.Math/exp (* (- (/ 1 2)) (* x x)))))

(defmacro BOUNTY-REWARD [] 10)
(defmacro BOUNTY-FOCUS [] 20)

(defn weight-function [config bounty-lines]
  (fn [x]
    (apply + (for [line bounty-lines]
               (* (or (config :bounty-reward)
                      (BOUNTY-REWARD))
                  (bellcurve (/ (- x line)
                                (or (config :bounty-focus)
                                    (BOUNTY-FOCUS)))))))))

(defn gcov-count-executed [report]
  (->> report vals (filter #(> % 0)) count))

(defn gcov-bounty-reward [config [report bounty-lines]]
  (let [bounty-function (weight-function config bounty-lines)]
    (apply +
           (for [[line count] report]
             (if (> count 0)
               (bounty-function line)
               0)))))

;;(gcov-count-executed (gcov-file (slurp "../yosys/yosys.cc.gcov")))

(defn measure-coverage [config tmpfile test]
  (log "Cleaning coverage statistics...")
  ;; remove any existing gcov files
  (sh "rm" (format "%s/*.gcov" tmpfile))
  ;; delete gcda files in advance of running test
  (->> config
       :src-dir
       clojure.java.io/file
       file-seq
       (map #(.getPath %))
       (map str)
       (filter #(re-matches #"\S+\.gcda" %))
       (pmap delete-file)
       doall)
  (log "Running test...")
  (let [result (test)]
    (log "Collecting coverage...")
    (run-gcov (find-src-files (config :src-dir)) tmpfile)
    (let [gcov-files (->> tmpfile
                          clojure.java.io/file
                          file-seq
                          (map str)
                          (filter #(re-matches #"\S+\.gcov" %)))
          coverage (pmap (fn [x] (->> x slurp gcov-file)) gcov-files)
          executed-lines (->> coverage
                              (pmap gcov-count-executed)
                              (apply +))
          total-lines (apply + (pmap count coverage))]
      (log (format "Test covered %s lines" executed-lines))
      (if (config :bounties)
        (let [src-files (mapv (fn [x] (last (str/split (str/replace x #"\.gcov$" "") #"\/"))) gcov-files)
              bounty-reward (->> (map (config :bounties) src-files)
                                 (map vector coverage)
                                 (pmap (partial gcov-bounty-reward config))
                                 (apply +))]
          (log (format "Test rewarded a bounty of %s" bounty-reward))
          [(+ executed-lines bounty-reward) result])
        [executed-lines result]))))

;;(measure-coverage "/Users/archie/yosys" "/tmp" (partial + 1 2))
