(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defmacro LINEEND [] #"\n")
(defmacro KEYWORD [] #"(?=(\.names|\.subckt|\.inputs|\.outputs|\.latch))")

(defmacro unique-id [] "0")
;; modules
(defn get-modules [s] (filter (fn [x] (str/includes? x ".model")) (str/split s #"(?=.model)")))
;;(defn modules [s] (str/split s #"(?=.model)"))
(defn get-inputs [s] (filter (fn [x] (str/includes? x ".inputs")) (str/split s (LINEEND))))
(defn get-outputs [s] (filter (fn [x] (str/includes? x ".outputs")) (str/split s (LINEEND))))
(defn get-names [s] (filter (fn [x] (str/includes? x ".names")) (str/split s (LINEEND))))
(defn get-subckt [s] (filter (fn [x] (str/includes? x ".subckt")) (str/split s (LINEEND))))
;; cells are either .names or .subckt
(defn cells [s] (filter (fn [x] (str/includes? x ".outputs")) (str/split s (LINEEND))))
(defn blocks [s] (filter (fn [x] (re-find (KEYWORD) x)) (str/split s (KEYWORD))))

(defn manage-input-node [name] {:type :input})
(defn manage-input-port [name] {:output name})
(defn manage-inputs [block [nodes edges]]
  (let [args (rest (str/split block #" "))]
    (let [new-nodes (map manage-input-node args)]
      (let [new-ports (map manage-input-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-output-node [name] {:type :output})
(defn manage-output-port [name] {:input name})
(defn manage-outputs [block [nodes edges]]
  (let [args (rest (str/split block #" "))]
    (let [new-nodes (map manage-output-node args)]
      (let [new-ports (map manage-output-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-names [block [nodes edges]]
  (let [lines (str/split block (LINEEND))]
    (let [table-rows (rest lines)]
      (case table-rows
        [] [(conj nodes {:type :constant :value :false}) edges]
        ["1"] [(conj nodes {:type :constant :value :true}) edges]
        (let [args (rest (str/split (first lines) #" "))]
          (let [output (last args)]
            (let [inputs (butlast args)]
              [(conj nodes {:type :names :table table-rows}) (conj edges {:inputs inputs :output output})])))))))

(defn manage-latch [block [nodes edges]]
  (let [args (rest (str/split block #" "))]
    (let [[input output type clock initial] args]
      [(conj nodes {:type :latch :trigger-type type :initial initial}) (conj edges {:input input :output output :clk clock})])))

(defn manage-subckt [block [nodes edges]]
  (let [args (drop 1 (str/split block #" "))]
    (let [model-name (first args)]
      (let [port-connections (rest args)]
        (let [node-name (str model-name (unique-id))]
          (let [new-edges (map (fn [x] (let [[port source] (str/split x #"=")] {source (str node-name "." port)})) port-connections)]
            [(concat nodes [node-name]) (concat edges new-edges)]))))))

(defn manage [block [nodes edges]]
  (let [keyword (first (str/split block #" "))]
    (case keyword
      ".inputs" (manage-inputs block [nodes edges])
      ".outputs" (manage-outputs block [nodes edges])
      ".names" (manage-names block [nodes edges])
      ".subckt" (manage-subckt block [nodes edges])
      ".latch" (manage-latch block [nodes edges])
      )))

(defn parse' [blocks [nodes edges]]
  (case blocks
    nil [nodes edges]
    (let [[head & tail] blocks]
      (parse' tail (manage head [nodes edges])))))
(defn parse [s] (parse' (blocks s) [[] []]))

;;(print (manage-inputs ".inputs a b c d" [[] []]))
;;(print "------\n")
;;(print (manage-subckt ".subckt adder b=2 c=3 d=2" [[] []]))
;;(print "------\n")
;;(print (manage-latch ".latch x y re z 2" [[] []]))
;;(print "------\n")
;;(print (manage-names ".name x y z" [[] []]))
;;(print "------\n")
;;(print (manage-names ".name x y z \n 1" [[] []]))
;;(print "------\n")
;;(print (manage-names ".names x y z \n 1 1 1" [[] []]))
;;(print "------\n")
;;(print (manage-outputs ".outputs x y z" [[] []]))
;;(print "------\n")
;;(print (manage-inputs ".inputs x y z" [[] []]))


;;(print (parse (slurp "i.cl.blif")))

;;(print (let [[nodes ports] (parse (slurp "i.cl.blif"))] (let [vars (map vals ports)] (reduce concat vars))))
(defn get-vars [ports]
  (let [vars (map vals ports)] (reduce concat vars)))
;; to find edge genes, iterate through variables, find edges where they connect

;; discover-source - finds nodes which have a port which uses this variable
;; discover-sink - finds nodes which have a port which uses this variable
(defn discover-edge [var ports]
  {:post [(< (count %) 3)]}  ;; Post condition that lenght of result is two or fewer
  (filter identity (map-indexed (fn [index port] (if (.contains (vals port) var) index nil)) ports)))

(print (let [[nodes ports] (parse (slurp "example.blif"))] (discover-edge "clk" ports)))

(defn ports-to-edges [ports]
  (map (fn [x] (discover-edge x ports)) (get-vars ports)))

(defn genetic-representation [f]
  (let [[nodes ports] (parse (slurp f))]
    (let [edges (ports-to-edges ports)]
      [nodes edges])))

(print (genetic-representation "example.blif"))

;;(defn genetic-to-blif [nodes edges])
;; when converting from genetic representation to blif
;; each edge is represented by a variable (note importance of which argument it is)

(defn edge-to-var [edge]
  {:pre (< (count edge) 3)
   :post (string? %)}
  (format "$%s" (str/replace (str/replace (pr-str edge) #"\s" "_") #"\(|\)" "")))

(print (edge-to-var '(1 2)))

(defn generate-input [node index edges]
  (format ".inputs %s" (edge-to-var (first (filter (fn [x] (.contains x index)) edges)))))
(defn generate-output [node index edges]
  (format ".outputs %s" (edge-to-var (first (filter (fn [x] (.contains x index)) edges)))))
(defn generate-latch [node index edges] (format ".latch %s %s" (get node :trigger-type) (get node :initial)))
(defn generate-constant [node index edges] ".names")
(defn generate-names [node index edges] (format ".names \n %s" (get node :table)))
(defn generate [node index edges]
  (case (get node :type)
    :input (generate-input node index edges)
    :output (generate-output node index edges)
    :latch (generate-latch node index edges)
    :constant (generate-constant node index edges)
    :names (generate-names node index edges)
    (throw "Unrecognised Node Type encountered during BLIF generation.")
))

(print (let [[nodes edges] (genetic-representation "example.blif")]
       (map-indexed (fn [index node] (generate node index edges)) nodes)))
