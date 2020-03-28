(ns cipher-sabre.cipher
  (:require [clojure.edn :as edn])
  (:import java.security.SecureRandom))

(def ^:private default-cycles 20)
(def ^:private range-256 (range 256))
(def ^:private range-256-vec (vec range-256))

(defn random
  [n]
  (let [sr (SecureRandom/getInstance "NativePRNG")
        buffer (byte-array n)
        _ (.nextBytes sr buffer)]
    (map #(mod % 256) buffer))) 

(defn- swapv!
  "Swap the position `pos-a` and `pos-b` on a transient vector `v`."
  [v pos-a pos-b]
  {:pre [(and (< -1 pos-a (count v)) (< -1 pos-b (count v)))]}
  (let [a (get v pos-b)
        b (get v pos-a)]
    (assoc! v pos-a a pos-b b)))

(defn- initialise-state
  "Shuffle state 256 times."
  [{initial-state :state initial-j :j} key]
  {:pre [(and (vector? key) (vector? initial-state))]}
  (let [key-length (count key)]
    ;; `i` and `j` are RC4 terminology
    (loop [state (transient initial-state)
           positions range-256
           j initial-j] 
      (if (seq positions)
        (let [i (first positions)
              j' (mod (+ j
                         (get state i)
                         (int (get key (mod i key-length))))
                      256)]
          (recur
           (swapv! state i j')
           (rest positions)
           j'))
        {:state (persistent! state)
         :j j}))))

(defn- initialise-with-n-cycles
  "Initialise the state SBox cycling it n times."
  ([key iv] (initialise-with-n-cycles key iv default-cycles))
  ([key iv cycles]
   (let [key (vec (concat key iv))]
     (loop [c cycles internal-state {:state range-256-vec :j 0}]
       (if (pos? c)
         (recur (dec c) (initialise-state internal-state key))
         (:state internal-state))))))

(defn- cipher
  "Cipher the `content` with the provided `state`."
  [initial-content initial-state]
  (loop [i 0
         j 0
         state (transient initial-state)
         content initial-content
         output []]
    (if (seq content)
      (let [i' (mod (inc i) 256)
            j' (mod (+ j (get state i')) 256)
            state' (swapv! state i' j')
            t (mod (+ (get state' i')
                      (get state' j'))
                   256)
            cipher-datum (get state' t)]
        (recur i'
               j'
               state'
               (rest content)
               (conj output (bit-xor cipher-datum (first content)))))
      output)))

(defn init-and-cipher
  [content key iv cycles]
  (cipher content
          (if (seq cycles)
            (initialise-with-n-cycles key iv (edn/read-string cycles))
            (initialise-with-n-cycles key iv))))
