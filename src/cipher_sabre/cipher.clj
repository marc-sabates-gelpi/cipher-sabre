(ns cipher-sabre.cipher
  (:require [clojure.edn :as edn])
  (:import java.security.SecureRandom))

(def ^:private range-256 (range 256))
(def ^:private range-256-vec (vec range-256))

(defn random
  [n]
  (let [sr (SecureRandom/getInstance "NativePRNG")
        buffer (byte-array n)
        _ (.nextBytes sr buffer)]
    (map #(mod % 256) buffer))) 

(defn- swapv
  "Swap the position `pos-a` and `pos-b` on a vector `v`."
  [v pos-a pos-b]
  {:pre [(and (< -1 pos-a (count v)) (< -1 pos-b (count v)))]}
  (-> v
      (assoc pos-a (get v pos-b))
      (assoc pos-b (get v pos-a))))

(defn- initialise-state
  "Shuffle state 256 times."
  [{initial-state :state initial-j :j} key]
  {:pre [(and (vector? key) (vector? initial-state))]}
  (let [key-length (count key)]
    ;; `i` and `j` are RC4 terminology
    (loop [state initial-state
           positions range-256
           j initial-j] 
      (if (seq positions)
        (let [i (first positions)
              j' (mod (+ j
                         (get state i)
                         (int (get key (mod i key-length))))
                      256)]
          (recur
           (swapv state i j')
           (rest positions)
           j'))
        {:state state
         :j j}))))

(defn- initialise-with-n-cycles
  "Initialise the state SBox cycling it n times."
  ([key iv] (initialise-with-n-cycles key iv 1))
  ([key iv cycles]
   (let [key (vec (concat key iv))]
     (loop [c cycles internal-state {:state range-256-vec :j 0}]
       (if (pos? c)
         (recur (dec c) (initialise-state internal-state key))
         (:state internal-state))))))

(defn- cipher
  "Cipher the `content` with the provided `state`."
  [initial-content initial-state]
  (loop [current-pos 0
         rand-pos 0
         state initial-state
         content initial-content
         output []]
    (if (seq content)
      (let [updated-current-pos (-> current-pos inc (mod 256))
            updated-rand-pos (-> rand-pos (+ (get state updated-current-pos)) (mod 256))
            updated-state (swapv state updated-current-pos updated-rand-pos)
            temp-pos (-> (+ (get updated-state updated-current-pos) (get updated-state updated-rand-pos)) (mod 256))
            cipher-datum (get updated-state temp-pos)]
        (recur updated-current-pos
               updated-rand-pos
               updated-state
               (rest content)
               (conj output (bit-xor cipher-datum (first content)))))
      output)))

(defn init-and-cipher
  [content key iv cycles]
  (cipher content
                 (if (seq cycles)
                   (initialise-with-n-cycles key iv (edn/read-string cycles))
                   (initialise-with-n-cycles key iv))))
