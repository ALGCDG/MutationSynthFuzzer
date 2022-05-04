(ns pure-random)

(defn pure-rand-nth [seed col]
  (-> seed
      java.util.Random.
      .nextInt
      Math/abs
      (mod ,,, (count col))
      #_(nth col)))

(defn pure-sample [seed probability col]
  (->> seed
       java.util.Random.
       .doubles
       .iterator
       iterator-seq
       (take (count col))
       (map vector col)
       (filter #(<= (second %) probability))))

;;(mod (count '(1 2 3 4 5 6 7 8)) 0)
;;
;;(pure-rand-nth 0 '(1 2 3 4 5 6 7 8))

(defn generate-seeds [seed n]
  (->> seed
       java.util.Random.
       .ints
       .iterator
       iterator-seq
       (take n)))
