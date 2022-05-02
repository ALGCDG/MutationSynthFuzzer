#! /usr/bin/env clojure

(require '[clojure.string :as str])

(defn get-modules [s] (filter (fn [x] (str/includes? x ".model")) (str/split s #"(?=\.model)")))

(defn get-name [filename]
  (first (str/split (last (str/split filename #"\/")) #"\.blif$")))

(defn module-filename [output-dir filename id]
  (format "%s/%s.%X.blif" output-dir (get-name filename) id))

(defn create-module [output-dir filename module]
  (spit (module-filename output-dir filename (hash module)) module))

(defn separate-modules [output-dir filename]
  (println (format "Separating %s..." filename))
  (->> (slurp filename)
       get-modules
       (filter #(-> % (str/includes? ".subckt") not))
       (mapv (partial create-module output-dir (str filename)))))

(defn prepare-corpus [input-dir output-dir]
  (->> input-dir
       clojure.java.io/file
       file-seq
       (filter #(str/includes? % "blif"))
       (mapv (partial separate-modules output-dir))))

(prepare-corpus (first *command-line-args*) (second *command-line-args*))
