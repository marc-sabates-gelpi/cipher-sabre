(ns cipher-sabre.cipher)

(defn- random-chars
  [n]
  (let [chars (map char (range 33 127))
        text (take n (repeatedly #(rand-nth chars)))]
    (reduce str text))) 

(defn- swapv
  "Swap the position `i` and `j` on a vector `v`."
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
         :initialisation-vector initialisation-vector}
        (recur
         (rest c)
         (initialise-state state key))))))
