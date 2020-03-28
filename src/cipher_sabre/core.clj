(ns cipher-sabre.core
  (:require [byte-streams :as streams]
            [cipher-sabre.cipher :as cipher]
            [clojure.java.io :refer [file output-stream]]
            [clojure.edn :as edn]))

(defn write-binary-file
  [s v]
  (with-open [out (output-stream (file s))]
    (.write out (byte-array (count v) v))))

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

(defn cipher-command
  ([key path]
   (cipher-command key path nil))
  ([key path cycles]
   (let [iv (cipher/random 10)]
     (->> (cipher/init-and-cipher (text-file->ints path) key iv cycles)
          (concat (map int iv))
          (write-binary-file "ciphered.out")))))

(defn decipher-command
  ([key path]
   (decipher-command key path nil))
  ([key path cycles]
   (let [all (map int (read-ciphered-file path))
         iv (take 10 all)
         content (drop 10 all)]
     (->> (cipher/init-and-cipher content key iv cycles)
          (write-binary-file "clear-text.out")))))

(defn run-command
  [command key path cycles]
  (case command
    "cipher" (cipher-command key path cycles)
    "decipher" (decipher-command key path cycles)
    nil))

(defn -main
  "Entry point for CipherSabre."
  [command key path & [cycles]]
  (if (and (seq key) (seq path))
    (run-command command key path cycles)
    (println "key and file are mandatory.. Try again with clj -Arun \"key\" \"file\"")))
