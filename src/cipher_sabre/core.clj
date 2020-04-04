(ns cipher-sabre.core
  (:require [cipher-sabre.cipher :as cipher]
            [cipher-sabre.file-io :as file-io]))

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
          (file-io/write-hex-file "ciphered.out")))))

(defn decipher-command
  ([key path]
   (decipher-command key path nil))
  ([key path cycles]
   (let [all (map int (file-io/read-ciphered-file path))
         iv (take 10 all)
         content (drop 10 all)]
     (->> (cipher/init-and-cipher content key iv cycles)
          (file-io/write-binary-file "clear-text.out")))))

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
