(ns util)

(defn input-keyword [index] (keyword (format "input%d" index)))

(defn enumerate [col] (map-indexed vector col))

(defn date-time []
  (.format (java.text.SimpleDateFormat. "dd/MM/yyyy-HH:mm:ss")
           (new java.util.Date)))

(defn log [x]
  (println (format "[%s] %s" (date-time) x)))
