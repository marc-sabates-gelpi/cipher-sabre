(ns cipher-sabre.io
  (:require [byte-streams :as streams]
            [clojure.java.io :refer [file output-stream]]
            [clojure.edn :as edn]))

(defn write-binary-file
  [path content]
  (with-open [out (output-stream (file path))]
    (.write out (byte-array (count content) content))))

(defn read-binary-file
  [path]
  (streams/to-byte-array (file path)))

(defn- throw-if
  "Throw `Exception` when (pred x) is `true`; otherwise return `x`."
  [pred x]
  (if (pred x)
    (throw (Exception. (format "Exception for %s" x)))
    x))

(defn- bin-hex?
  [x]
  (or (<= 48 x 57) ;; 0-9
      (<= 97 x 102) ;; a-f
      (<= 65 x 70) ;; A-F
      ))

(defn read-hex-file
  "Process an hex file until it reaches the end; throw `Exception` if no hex char found."
  [path]
  (sequence (comp (remove #{\newline})
                  (map (partial throw-if (complement (comp bin-hex? int))))
                  (partition-all 2)
                  (map (partial apply str "0x"))
                  (map edn/read-string))
            (slurp path)))

(defn read-ciphered-file
  [path]
  (try (read-hex-file path)
       (catch Exception _ (read-binary-file path))))

(defn- text-file->ints
  [path]
  (map int (slurp path)))

(defn write-hex-file
  [path content]
  (spit path (apply str (sequence (comp (map #(mod % 256))
                                        (map (partial format "%02x")))
                                  content))))
