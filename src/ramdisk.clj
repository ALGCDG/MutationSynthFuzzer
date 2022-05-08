(ns ramdisk
  (:require [util :refer [log]])
  (:require [clojure.string :as str])
  (:require [clojure.java.shell :refer [sh]]))

;; Creates and mounts a ramdisk
;; Behaviour is specialised for OS.

;;(defn mount-linux [size]
;;  (sh "bash" "-c" (format "mount -o size=%d -t tmpfs none %s" size file)))

(defn mount-linux []
  (log "Creating memory mapped directory in Linux...")
  (let [file (->> "/dev/shm"
                  clojure.java.io/file
                  file-seq
                  hash
                  (format "/dev/shm/fuzzmount%X"))]
    (sh "mkdir" file)
    (log (format "Creating directory %s..." file))
    (log "Created memory mapped directory in Linux!")
    [file nil]))

(defn mount-macos [size]
  (log "Mounting ramdisk in MacOS...")
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
    (log (format "Using device %s, mounting into %s, size %.2f MB..." device file MB))
    (sh "bash" "-c" (format "newfs_hfs %s" device))
    (sh "bash" "-c" (format "mkdir %s" file))
    (sh "bash" "-c" (format "mount -t hfs %s %s" device file))
    (log "Mounted ramdisk in MacOS!")
    [file device]))

(defn unmount-linux [file]
  (log "Removed memory mapped directory in Linux...")
  (sh "rm" "-rf" file)
  (log "Removed memory mapped directory in Linux!"))

(defn unmount-macos [[file device]]
  (log "Unmounting ramdisk in MacOS...")
  (sh "bash" "-c" (format "umount %s" file))
  (sh "bash" "-c" (format "hdiutil detach %s" device))
  (sh "bash" "-c" (format "rm -r %s" file))
  (log "Unmounted ramdisk in MacOS!"))

(defn unmount [[file device]]
  (case (System/getProperty "os.name")
    "Mac OS X" (unmount-macos [file device])
    "Linux" (unmount-linux file)))

(defn mount [size]
  (case (System/getProperty "os.name")
    "Mac OS X" (mount-macos size)
    "Linux" (mount-linux)))

(defn use-ramdisk "To use ramdisk, provide function which uses the created ramdisk"
  [size func]
  (let [resource (mount size)]
    (try (func resource)
         (finally (unmount resource)))))

;;(unmount-macos (mount-macos (* 512 128000)))
