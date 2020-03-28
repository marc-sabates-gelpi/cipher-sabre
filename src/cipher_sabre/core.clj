(ns cipher-sabre.core
  (:require [cipher-sabre.cipher :as cipher]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.java.io :refer [file output-stream input-stream]]
            [taoensso.timbre :refer [spy debug]]
            [byte-streams :as streams])
  (:import  [java.nio ByteBuffer]))

(defn write-binary-file
  [s v]
  (with-open [out (output-stream (file s))]
    (.write out (byte-array (count v) v))))

(defn read-binary-file
  [s]
  (streams/to-byte-array (file s)))

(defn cipher-command
  ([key file]
   (cipher-command key file nil))
  ([key file cycles]
   (let [iv (cipher/random 10)]
     (->> (cipher/init-and-cipher (map int (slurp file)) key iv cycles)
          (concat (map int iv))
          (write-binary-file "ciphered.out")))))

(defn decipher-command
  ([key file]
   (decipher-command key file nil))
  ([key file cycles]
   (let [all (map int (read-binary-file file))
         iv (take 10 all)
         content (drop 10 all)]
     (->> (cipher/init-and-cipher content key iv cycles)
          (write-binary-file "clear-text.out")))))

(defn run-command
  [command key file cycles]
  (case command
    "cipher" (cipher-command key file cycles)
    "decipher" (decipher-command key file cycles)
    nil))

(defn -main
  "Entry point for CipherSabre."
  [command key file & [cycles]]
  (if (and (seq key) (seq file))
    (run-command command key file cycles)
    (println "key and file are mandatory.. Try again with clj -m cipher-sabre.core \"key\" \"file\"")))
