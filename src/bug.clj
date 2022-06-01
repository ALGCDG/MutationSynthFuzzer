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

(defn tree-bug [bug]
  (-> bug :input (pp/write ,,, :dispatch pp/code-dispatch)))

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

(defn is-synth-fail? [bug] (->> bug :error :type (= :synth-fail)))
(defn is-equiv-fail? [bug] (->> bug :error :type (= :equiv-fail)))
(defn is-timeout? [bug] (case (->> bug :error :type)
                          :synth-fail (and (->> bug :error :result :exit (= 124))
                                           (->> bug :error :result :err empty?))
                          :equiv-fail (and (->> bug :error :proof :exit (= 124))
                                           (->> bug :error :proof :err empty?))
                          nil))
(defn is-proof-err? [bug] (and (->> bug :error :type (= :equiv-fail))
                               (->> bug :error :proof :exit (= 16))))

(defn asan-err? [bug] (case (->> bug :error :type)
                        :equiv-fail (->> bug :error :proof :out (re-find #"AddressSanitizer"))
                        :synth-fail (->> bug :error :result :out (re-find #"AddressSanitizer"))
                        nil))
(defn classify-bugs [bugs]
  {:synth-fail (filter is-synth-fail? bugs)
   :equiv-fail (filter is-equiv-fail? bugs)
   :timeouts (filter is-timeout? bugs)
   :proof-err (filter is-proof-err? bugs)
   :asan-err (filter asan-err? bugs)})

(defn simulate-bug [config bug]
  (let [sim-dir (->> (format "bugs/%X_sim" (->> bug :input hash))
                     java.io.File.
                     .getAbsolutePath
                     str)
        post-path "sut_post.v"
        pre-path "sut_pre.v"]
    #_(.mkdir (java.io.File. sim-dir))
    (spit post-path (->> bug :error :post-synth-verilog))
    (spit pre-path (->> bug :error :pre-synth-verilog))
    (let [sim-result (simulate (assoc config :id (->> bug :input hash (format "%X"))) (->> bug :input eval) "." pre-path post-path)]
      #_(if sim-result
          (do
            (spit (format "%s/log.out.txt" sim-dir) (sim-result :out))
            (spit (format "%s/log.err.txt" sim-dir) (sim-result :err))
            (spit (format "%s/exit.txt" sim-dir) (sim-result :exit))))
      sim-result)))

(def bugs (read-bugs "bugs"))

(defn index-bugs [bugs] (->> bugs
                             (mapv (fn [sim-bug] {(->> sim-bug :input hash (format "%X")) sim-bug}))
                             (apply merge)))

(def hashed-bugs (index-bugs bugs))

(let [grouped-bugs (classify-bugs bugs)]
  (println (format "%s Bug Reports" (->> bugs count)))
  (println (format "%s Synthesis Bugs" (->> grouped-bugs :synth-fail count)))
  (println (format "%s Equivalence Bugs" (->> grouped-bugs :equiv-fail count)))
  (println (format "%s Timeouts" (->> grouped-bugs :timeouts count)))
  (println (format "%s Proof Errors" (->> grouped-bugs :proof-err count)))
  (println (format "%s ASan Errors" (->> grouped-bugs :asan-err count))))

(def simulation-config (merge {:smtbmc-path "/Users/archie/yosys/yosys-smtbmc"}
                              (->> "config.clj" slurp read-string)))

(defn tree-depth [count root]
  (if (->> root (filter list?) empty? not)
    (->> root
         (filter list?)
         (map (partial tree-depth (+ 1 count)))
         (apply max))
    (+ 1 count)))

(defn verisim [bugs] (->> bugs
                          classify-bugs
                          :equiv-fail
                          #_(take 100)
                          (mapv (fn [bug]
                                  (merge bug
                                         (try (simulate-bug simulation-config bug)
                                              (catch Exception e (println "error during simulation: " e))))))
                          #_(filter (fn [[bug result]] (->> result :out empty? not)))
                          index-bugs))
