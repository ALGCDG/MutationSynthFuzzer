(ns util)

(defn input-keyword [index] (keyword (format "input%d" index)))

(defn enumerate [col] (map-indexed vector col))
