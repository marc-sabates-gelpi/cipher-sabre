(ns cipher-sabre.core
  (:require [cipher-sabre.cipher :as cipher]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(defn -main
  "Entry point for CipherSabre."
  [& [key file cycles]]
  (if (not-any? string/blank? (list key file))
    (cipher/initialise-with-n-cycles key (edn/read-string cycles))
    (println "key and file are mandatory.. Try again with clj -m cipher-sabre.core \"key\" \"file\"")))
