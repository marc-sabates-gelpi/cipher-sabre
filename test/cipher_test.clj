(ns cipher-sabre.cipher-test
  (:require [clojure.test :refer :all]
            [cipher-sabre.cipher :as cipher]))

(deftest random-chars
  (testing "consecutive calls differ"
    (is (not= (#'cipher/random 10)
              (#'cipher/random 10)
              (#'cipher/random 10))))

  (testing "correct length"
    (is (= 15
           (count (#'cipher/random 15))))))

(deftest swapv!
  (testing "result is a vector"
    (is (= clojure.lang.PersistentVector$TransientVector
           (type (#'cipher/swapv! (transient [1 2 3]) 1 2)))))

  (testing "swaps properly"
    (is (let [v [1 2 3 4]
              v! (transient v)
              x 1
              y 2
              v' (persistent! (#'cipher/swapv! v! x y))]
          (and (= (get v x) (get v' y))
               (= (get v y) (get v' x))))))

  (testing "leaves the rest intact"
    (is (let [v [1 2 3 4 5 6]
              v! (transient v)]
          (= (subvec v 2)
             (subvec (persistent! (#'cipher/swapv! v! 0 1)) 2)))))

  (testing "boundaries"
    (is (let [v (transient [1 2 3 4])]
          (= [4 2 3 1]
             (persistent! (#'cipher/swapv! v 0 3))))))

  (testing "out of boundaries"
    (is (thrown? AssertionError (#'cipher/swapv! (transient [1 2]) -1 0)))
    (is (thrown? AssertionError (#'cipher/swapv! (transient [1 2]) 0 2)))))
