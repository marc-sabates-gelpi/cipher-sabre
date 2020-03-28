(ns cipher-sabre.cipher
  (:require [clojure.edn :as edn]
            [taoensso.timbre :refer [debug spy]]))

(def ^:private characters (map char (range 33 127)))
(def ^:private range-256 (range 256))
(def ^:private range-256-vec (vec range-256))

(defn random-chars
  [n]
  (take n (repeatedly #(rand-nth characters)))) 

(defn- swapv
  "Swap the position `pos-a` and `pos-b` on a vector `v`."
  [v pos-a pos-b]
  {:pre [(and (< -1 pos-a (count v)) (< -1 pos-b (count v)))]}
  (-> v
      (assoc pos-a (get v pos-b))
      (assoc pos-b (get v pos-a))))

(defn- initialise-state
  "Shuffle state 256 times."
  [initial-state key]
  {:pre [(and (vector? key) (vector? initial-state))]}
  (let [key-length (count key)]
    ;; `i` and `j` are RC4 terminology
    (loop [state initial-state
           positions range-256
           j 0 ;; FIXME: CipherSabre-2 would init to 0 only once
           ] 
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
        state))))

(defn- initialise-with-n-cycles
  "Initialise the state SBox cycling it n times."
  ([key iv] (initialise-with-n-cycles key iv 1))
  ([key iv cycles]
   (let [key (vec (concat key iv))]
     (loop [c cycles state range-256-vec]
       (if (pos? c)
         (recur (dec c) (initialise-state state key))
         state)))))

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
