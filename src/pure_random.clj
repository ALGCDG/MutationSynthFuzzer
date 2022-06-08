(ns pure-random)

(defn pure-rand-nth [seed col]
  (if (not (empty? col))
    (-> seed
        java.util.Random.
        .nextInt
        Math/abs
        (mod ,,, (count col))
        ((into [] col)))))

(defn pure-sample [seed probability col]
  (->> seed
       java.util.Random.
       .doubles
       .iterator
       iterator-seq
       (map vector col)
       (filter #(<= (second %) probability))
       (map first)))

;;(mod (count '(1 2 3 4 5 6 7 8)) 0)
;;
;;(pure-rand-nth 0 [1 2 3 4 5 6 7 8])
;;
;;(pure-sample 0 0.5 [1 2 3 4 5 6 7 8])
;;
;;(take 10 (pure-sample 0 0.5 (range)))

(defn generate-seeds [seed n]
  (->> seed
       java.util.Random.
       .ints
       .iterator
       iterator-seq
       (take n)
       doall))

(generate-seeds 0 10)
