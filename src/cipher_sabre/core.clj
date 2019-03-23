(ns cipher-sabre.core
  (:require [cipher-sabre.cipher :as cipher]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(defn -main
  "Entry point for CipherSabre."
  [& [key file cycles]]
  (if (not-any? string/blank? (list key file))
    (let [{:keys [initialisation-vector] :as internal-state} (cipher/initialise-with-n-cycles key (edn/read-string cycles))]
      (spit "ciphered.out" (apply str (interpose " " (concat (mapv int initialisation-vector) (cipher/cipher (slurp file) internal-state))))))
    (println "key and file are mandatory.. Try again with clj -m cipher-sabre.core \"key\" \"file\"")))
