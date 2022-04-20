(ns ramdisk
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]]))

;; Creates and mounts a ramdisk
;; Behaviour is specialised for OS.

;;(defn mount-linux [size]
;;  (sh "bash" "-c" (format "mount -o size=%d -t tmpfs none %s" size file)))

(defn mount-macos [size]
  (let [numsectors (/ size 512)
        MB (/ (float size) 1000000)
        device (->> numsectors
                    (format "hdiutil attach -nomount ram://%d")
                    (sh "bash" "-c")
                    :out
                    str/trim)
        file (->> "/tmp"
                  clojure.java.io/file
                  file-seq
                  hash
                  (format "/tmp/fuzzmount%X"))]
    (println (format "Using device %s, mounting into %s, size %.2f MB..." device file MB))
    (sh "bash" "-c" (format "newfs_hfs %s" device))
    (sh "bash" "-c" (format "mkdir %s" file))
    (sh "bash" "-c" (format "mount -t hfs %s %s" device file))
    [file device]))

(defn unmount-macos [[file device]]
  (sh "bash" "-c" (format "umount %s" file)
  (sh "bash" "-c" (format "hdiutil detach %s" device))
  (sh "bash" "-c" (format "rm -r %s" file))))

(defn use-ramdisk "To use ramdisk, provide function which uses the created ramdisk"
  [size func]
  (->>  size
        mount-macos
        func
        unmount-macos))

;;(unmount-macos (mount-macos (* 512 128000)))
