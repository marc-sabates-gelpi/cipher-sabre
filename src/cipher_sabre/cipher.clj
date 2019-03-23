(ns cipher-sabre.cipher)

(defn- random-chars
  [n]
  (let [chars (map char (range 33 127))
        text (take n (repeatedly #(rand-nth chars)))]
    (reduce str text))) 

(defn- swapv
  "Swap the position `pos-a` and `pos-b` on a vector `v`."
  [v pos-a pos-b]
  {:pre [(and (< -1 pos-a (count v)) (< -1 pos-b (count v)))]}
  (-> v
      (assoc pos-a (get v pos-b))
      (assoc pos-b (get v pos-a))))

(defn- initialise-state
  "Shuffle state 256 times to ready it for ciphering."
  [initial-state key]
  (let [key-length (count key)]
    (loop [state initial-state
           all-positions (range 256)
           rand-pos 0]
      (if-let [current-pos (first all-positions)]
        (let [updated-rand-pos (-> (get key (mod current-pos key-length))
                                   int
                                   (+ rand-pos (get state current-pos))
                                   (mod 256))]
          (recur
           (swapv state current-pos updated-rand-pos)
           (next all-positions)
           updated-rand-pos))
        state))))

(defn initialise-with-n-cycles
  "Initialise the state SBox cycling it n times."
  [key & [cycles]]
  (let [cycles (if (number? cycles)
                 cycles
                 20)
        initialisation-vector (random-chars 10)
        key (vec (concat key initialisation-vector))]
    (loop [c (range cycles)
           state (vec (range 256))]
      (if (empty? c)
        {:state state
         :initialisation-vector initialisation-vector
         :cipher-key key}
        (recur
         (rest c)
         (initialise-state state key))))))

(defn cipher
  "Cipher the `content` with the provided `params`."
  [initial-content {initial-state :state :as _params}]
  (loop [current-pos 0
         rand-pos 0
         state initial-state
         content (mapv int initial-content)
         output []]
    (if (empty? content)
      output
      (let [updated-current-pos (-> current-pos inc (mod 256))
            updated-rand-pos (-> rand-pos (+ (get state updated-current-pos)) (mod 256))
            updated-state (swapv state updated-current-pos updated-rand-pos)
            temp-pos (-> (+ (get updated-state updated-current-pos) (get updated-state updated-rand-pos)) (mod 256))
            cipher-datum (get updated-state temp-pos)]
        (recur updated-current-pos
               updated-rand-pos
               updated-state
               (vec (next content))
               (conj output (bit-xor cipher-datum (first content))))))))
