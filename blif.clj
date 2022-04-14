(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.pprint :as pp])

(defmacro LINEEND [] #"\n+")
(defmacro KEYWORD [] #"(?=(\.names|\.subckt|\.inputs|\.outputs|\.latch|\.end))")
(defmacro CNF-SYMBOLS [] ["0" "1" "-"])
(defmacro MISSING-EDGE [] :missing_edge)

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

(defn check-unimplemented [s]
  (let [lines (map str/trim (str/split s (LINEEND)))]
    (let [keywords (filter identity (map (partial re-matches #"^\.\S+") lines))]
      (let [unrecognised (filter #(not (re-find (KEYWORD) %)) keywords)]
        (if (not (empty? unrecognised))
          (binding [*out* *err*]
            (println (format "Unrecognised Keywords: %s" (str/join " " unrecognised)))))))))

(defn manage-input-node [name] {:type :input})
(defn manage-input-port [name] {name :output})
(defn manage-inputs [block [nodes edges]]
  (let [args (rest (str/split (str/trim block) #"(\s|\n)+"))]
    (let [new-nodes (map manage-input-node args)]
      (let [new-ports (map manage-input-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn manage-output-node [name] {:type :output})
(defn manage-output-port [name] {name :input})
(defn manage-outputs [block [nodes edges]]
  (let [args (rest (str/split (str/trim block) #"(\s|\n)+"))]
    (let [new-nodes (map manage-output-node args)]
      (let [new-ports (map manage-output-port args)]
        [(into [] (concat nodes new-nodes)) (into [] (concat edges new-ports))]))))

(defn input-keyword [index] (keyword (format "input%d" index)))

(defn manage-names [block [nodes edges]]
  (let [lines (str/split block (LINEEND))]
    (let [table-rows (rest lines)
          args (rest (str/split (str/trim (first lines)) #"\s+"))]
      (let [output (last args)]
        (case table-rows
          [] [(conj nodes {:type :constant :value :false}) (conj edges {output :output})]
          ["1"] [(conj nodes {:type :constant :value :true}) (conj edges {output :output})]
          (let [inputs (butlast args)
                cnf (map #(str/split % #"\s+") table-rows)]
            (let [input-map (->> (map-indexed (fn [index variable] {variable (input-keyword index)}) inputs)
                                 (apply merge))]
              [(conj nodes {:type :names :num-inputs (count inputs) :table cnf})
               (conj edges (merge input-map {output :output}))])))))))

(defn manage-latch [block [nodes edges]]
  (let [args (rest (str/split (str/trim block) #"(\s|\n)+"))]
    (let [[input output type clock initial] args]
      [(conj nodes {:type :latch :trigger-type type :initial (or initial "3")})
       (conj edges {input :input output :output clock :clk})])))

(defn manage-subckt [block [nodes edges]]
  (let [args (drop 1 (str/split (str/trim block) #"\s+"))]
    (let [model-name (first args)]
      (let [port-connections (rest args)]
        (let [node-name (str model-name (unique-id))]
          (let [new-edges (map (fn [x] (let [[port source] (str/split (str/trim x) #"=")] {source (str node-name "." port)})) port-connections)]
            [(concat nodes [node-name]) (concat edges new-edges)]))))))

(defn manage [block [nodes edges]]
  (let [keyword (first (str/split (str/trim block) #"\s+"))]
    (case keyword
      ".inputs" (manage-inputs block [nodes edges])
      ".outputs" (manage-outputs block [nodes edges])
      ".names" (manage-names block [nodes edges])
      ".subckt" (manage-subckt block [nodes edges])
      ".latch" (manage-latch block [nodes edges])
      [nodes edges])))

(defn parse' [blocks [nodes edges]]
  (case blocks
    nil [nodes edges]
    (let [[head & tail] blocks]
      (parse' tail (manage head [nodes edges])))))
(defn parse [s] (check-unimplemented s) (parse' (blocks s) [[] []]))

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
(defn discover-edge [ports var]
  {:post [(= 1 (count (filter (partial = :output) (vals %))))]}  ;; Post condition, any edge has only one output
  (->> (map-indexed (fn [index port] (if (contains? port var) {index (get port var)})) ports)
       (filter identity)
       (apply merge)))

;;(print (let [[nodes ports] (parse (slurp "example.blif"))] (discover-edge "clk" ports)))

(defn ports-to-edges [ports]
  (map (partial discover-edge ports) (get-vars ports)))

(defn genetic-representation [f]
  (let [[nodes ports] (parse (slurp f))]
    (let [edges (distinct (ports-to-edges ports))]
      [nodes edges])))

(pp/pprint (parse (slurp "example.blif")))

(print "\n--------\n")

(pp/pprint (genetic-representation "example.blif"))
;;(print (let [[n e] (genetic-representation "example.blif")] e))

;;(defn genetic-to-blif [nodes edges])
;; when converting from genetic representation to blif
;; each edge is represented by a variable (note importance of which argument it is)

(defn edge-to-var [edge]
  {:pre (< (count edge) 3)
   :post (string? %)}
  (format "$%s" (-> (keys edge)
                    (or ,,, (MISSING-EDGE))
                    pr-str
                    (str/replace ,,, #"\s" "_")
                    (str/replace ,,, #"\(|\)" ""))))

;;(print (edge-to-var '(1 2)))

(defn create-var [index port edges]
  (let [check
        (fn [edge]
          (and
           (.contains (keys edge) index)
           (= (get edge index) port)))]
    (let [edge (filter check edges)]
      ;;(assert (= (count edge) 1))
      ;;(print (format "  Test %s  " (pr-str edge)))
      (edge-to-var (first edge)))))

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

(defn generate-constant [node index edges]
  (let [table (if (= :true (get node :value)) "\n 1" "")]
    (format ".names %s %s" (create-var index :output edges) table)))

(defn generate-names [node index edges]
  (let [input-labels (map input-keyword (range (get node :num-inputs)))]
    ;;(print input-labels)
    (let [args (map #(create-var index % edges) input-labels)]
      ;;(print args)
      ;;(print (first args))
      ;;(print index)
      (let [output (create-var index :output edges)]
        ;;(print output)
        (format ".names %s %s \n %s"
                (str/join " " args)
                (create-var index :output edges)
                (str/join "\n" (map (partial str/join " ") (get node :table))))))))

(defn generate [node index edges]
  (case (get node :type)
    :input (generate-input node index edges)
    :output (generate-output node index edges)
    :latch (generate-latch node index edges)
    :constant (generate-constant node index edges)
    :names (generate-names node index edges)
    (throw "Unrecognised Node Type encountered during BLIF generation.")))

(print "\n--------\n")

(pp/pprint (let [[nodes edges] (genetic-representation "example.blif")]
             (map-indexed (fn [index node] (generate node index edges)) nodes)))

(map print (str/join "\n" (let [[nodes edges] (genetic-representation "example.blif")]
                            (map-indexed (fn [index node] (generate node index edges)) nodes))))

(defn generate-blif [[nodes edges]]
  (format "%s\n.names %s"
          (str/join "\n" (map-indexed (fn [index node] (generate node index edges)) nodes))
          (str (MISSING-EDGE))))

;; Trying out corssover and mutation operators

(defn enumerate [col] (map-indexed vector col))

(defn find-nodes-indexed [nodes type]
  (filter (fn [[index node]] (= (get node :type) type)) (enumerate nodes)))

(defn rand-nodetype [nodes type] (rand-nth (find-nodes-indexed nodes type)))

(defn change-latch-trigger [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :latch)]
    (let [other-triggers (set/difference #{:re, :fe, :as, :ah, :al} #{(get node :trigger-type)})]
      (let [new-node (assoc node :trigger-type (rand-nth (into [] other-triggers)))]
        [(assoc nodes modified-index new-node) edges]))))

(defn change-latch-initial [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :latch)]
    (let [other-initial (set/difference (set (range 4)) #{(get node :initial)})]
      (let [new-node (assoc node :initial (rand-nth (into [] other-initial)))]
        [(assoc nodes modified-index new-node) edges]))))

(defn change-constant-value [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :constant)]
    (let [new-node (assoc node :value (if (= (get node :value) :true) :false :true))]
      [(assoc nodes modified-index new-node) edges])))

;;(defn change-names-flip-term [[nodes edges]]
;;  (let [indexed-names-nodes (filter (fn [[index node]] (= (get node :type) :names)) (map-indexed vector nodes))]
;;    (let [[modified-index node] (rand-nth indexed-names-nodes)]
;;      (let [original-table (get node :table)]
;;        (let [new-table (rest (shuffle original-table))]
;;          (let [new-node (assoc node :table new-table)]
;;            [(assoc nodes modified-index new-node) edges]))))))

(defn change-names-remove-clause [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :names)]
    (let [original-table (get node :table)]
      (let [new-table (rest (shuffle original-table))]
        (let [new-node (assoc node :table new-table)]
          [(assoc nodes modified-index new-node) edges])))))

(defn change-names-add-clause [[nodes edges]]
  (let [[modified-index node] (rand-nodetype nodes :names)]
    (let [original-table (get node :table)]
      (let [n (+ 1 (get node :num-inputs))]
        (let [new-clause (take n (random-sample 0.1 (cycle (CNF-SYMBOLS))))]  ;; Note that the probability 0.1 is arbitrary, we are performing a uniform sample (just need to make sure probability is not 1).
          (let [new-table (conj original-table [new-clause])]
            (let [new-node (assoc node :table new-table)]
              [(assoc nodes modified-index new-node) edges])))))))

(defn update-edge [offset edge]
  (zipmap (map #(+ offset %) (keys edge)) (vals edge)))
;; TODO, implement properly, may require going back to modify edge representation (replacing one key with another is annoying

(defn update-edges [offset edges]
  (map (partial update-edge offset) edges))

(defn fix-edge [index-update output-indices edge]
  (apply (partial merge {}) (for [[index port] edge]
                              (if (contains? index-update index)
                                [(get index-update index) port]
                                (if (= port :output)
                                  [(rand-nth output-indices) port]
                                  [:missing port])))))

(pp/pprint (fix-edge {6 1 7 2 8 3} [] {6 :input, 7 :output, 8 :input0}))
(pp/pprint (fix-edge {6 1} [11 12 13 14] {6 :input, 7 :output, 8 :input0}))

(defn find-output-indices [nodes]
  (map first (filter (fn [[index node]] (not= (get node :type) :output)) (enumerate nodes))))

(defn dumb-crossover [[a-nodes a-edges] [b-nodes b-edges]]
  (let [b-offset-edges (update-edges (count a-nodes) b-edges)
        sampled-edges (random-sample 0.5 (concat a-edges b-offset-edges))
        [sampled-indices sampled-nodes] (->> (concat a-nodes b-nodes)
                                             enumerate
                                             (random-sample 0.5)
                                             (apply map vector))
        index-map (->> sampled-indices
                       enumerate
                       (apply (partial merge {}))
                       set/map-invert)
        edge-fixer (partial fix-edge index-map (find-output-indices sampled-nodes))]
    [sampled-nodes (map edge-fixer sampled-edges)]))

(print "\n--------\n")
(pp/pprint (dumb-crossover (genetic-representation "example.blif") (genetic-representation "example.blif")))

(print (generate-blif (dumb-crossover (genetic-representation "example.blif") (genetic-representation "example.blif"))))

(print "\n--------\n")
(pp/pprint (change-latch-trigger (genetic-representation "example.blif")))

(print "\n--------\n")
(pp/pprint (change-constant-value (genetic-representation "example.blif")))

(print "\n--------\n")
(pp/pprint (let [[n e] (genetic-representation "example.blif")] (update-edges 10 e)))

(print "\n--------\n")
(pp/pprint (change-names-add-clause (genetic-representation "example.blif")))
(print "\n--------\n")
(pp/pprint (change-names-remove-clause (genetic-representation "example.blif")))
;; TODO add sanity check that names must have at least one clause in its CNF (check when removing clauses)

(defmacro MUTATIONS [] '[change-latch-trigger
                         change-names-remove-clause
                         change-names-add-clause
                         change-constant-value
                         change-latch-initial])

(defn mutate [g] ((rand-nth (MUTATIONS)) g))

(pp/pprint (mutate (genetic-representation "example.blif")))

;;(print (generate-blif (-> (iterate mutate (genetic-representation "example.blif")) (nth 1000))))
