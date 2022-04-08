(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.pprint :as pp])

(defmacro LINEEND [] #"\n")
(defmacro KEYWORD [] #"(?=(\.names|\.subckt|\.inputs|\.outputs|\.latch|\.end))")

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
(defn manage-input-port [name] {name :output})
(defn manage-inputs [block [nodes edges]]
  (let [args (rest (str/split block #" "))]
    (let [new-nodes (map manage-input-node args)]
      (let [new-ports (map manage-input-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-output-node [name] {:type :output})
(defn manage-output-port [name] {name :input})
(defn manage-outputs [block [nodes edges]]
  (let [args (rest (str/split block #" "))]
    (let [new-nodes (map manage-output-node args)]
      (let [new-ports (map manage-output-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-names [block [nodes edges]]
  (let [lines (str/split block (LINEEND))]
    (let [table-rows (rest lines)]
      (let [args (rest (str/split (first lines) #" "))]
        (let [output (last args)]
          (case table-rows
            [] [(conj nodes {:type :constant :value :false}) (conj edges {output :output})]
            ["1"] [(conj nodes {:type :constant :value :true}) (conj edges {output :output})]
            (let [inputs (butlast args)]
              [(conj nodes {:type :names :table table-rows}) (conj edges {inputs :inputs output :output})])))))))

(defn manage-latch [block [nodes edges]]
  (let [args (rest (str/split block #" "))]
    (let [[input output type clock initial] args]
      [(conj nodes {:type :latch :trigger-type type :initial initial}) (conj edges {input :input output :output clock :clk})])))

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
      [nodes edges]
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
  (let [vars (map keys ports)] (reduce concat vars)))
;; to find edge genes, iterate through variables, find edges where they connect

;; discover-source - finds nodes which have a port which uses this variable
;; discover-sink - finds nodes which have a port which uses this variable
(defn discover-edge [var ports]
  {:post [(< (count %) 3)]}  ;; Post condition that lenght of result is two or fewer
  (filter identity (map-indexed (fn [index port] (if (.contains (keys port) var) {index (get port var)} nil)) ports)))

;;(print (let [[nodes ports] (parse (slurp "example.blif"))] (discover-edge "clk" ports)))

(defn ports-to-edges [ports]
  (map (fn [x] (discover-edge x ports)) (get-vars ports)))

(defn genetic-representation [f]
  (let [[nodes ports] (parse (slurp f))]
    (let [edges (distinct (ports-to-edges ports))]
      [nodes edges])))

(pp/pprint (genetic-representation "example.blif"))
;;(print (let [[n e] (genetic-representation "example.blif")] e))

;;(defn genetic-to-blif [nodes edges])
;; when converting from genetic representation to blif
;; each edge is represented by a variable (note importance of which argument it is)

(defn edge-to-var [edge]
  {:pre (< (count edge) 3)
   :post (string? %)}
  (format "$%s" (str/replace (str/replace (pr-str (flatten (map keys edge))) #"\s" "_") #"\(|\)" "")))

;;(print (edge-to-var '(1 2)))

(defn create-var [index port edges]
  (let [edge (filter (fn [x] (and (.contains (flatten (map keys x)) index) (.contains (flatten (map vals x)) port))) edges)]
    ;;(assert (= (count edge) 1))
    ;;(print (format "  Test %s  " (pr-str edge)))
    (edge-to-var (first edge))))

(defn generate-input [node index edges]
  (format ".inputs %s" (create-var index :output edges)))

(defn generate-output [node index edges]
  (format ".outputs %s" (create-var index :input edges)))

(defn generate-latch [node index edges]
  (let [input (create-var index :input edges)
        output (create-var index :output edges)
        clk (create-var index :clk edges)
        trigger (get node :trigger-type)
        initial (get node :initial)]
    (format ".latch %s %s %s %s %s" input output trigger clk initial)))

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

(print "\n--------\n")

(pp/pprint (let [[nodes edges] (genetic-representation "example.blif")]
       (map-indexed (fn [index node] (generate node index edges)) nodes)))
