(ns cipher-sabre.cipher-test
  (:require [clojure.test :refer :all]
            [cipher-sabre.cipher :as cipher]))

(deftest random-chars
  (testing "consecutive calls differ"
    (is (not= (#'cipher/random-chars 10)
              (#'cipher/random-chars 10)
              (#'cipher/random-chars 10))))

  (testing "correct length"
    (is (= 15
           (count (#'cipher/random-chars 15)))))

  ;; FIXME: This is probably not cryptographically secure
  #_(testing "no repeated elems < 256"
      (is (empty? (->> (#'cipher/random-chars 256)
                       frequencies
                       vals
                       (filter (partial < 1)))))))

(deftest swapv
  (testing "result is a vector"
    (is (vector? (#'cipher/swapv [1 2 3] 1 2))))

  (testing "swaps properly"
    (is (let [v [1 2 3 4]
              x 1
              y 2
              v' (#'cipher/swapv v x y)]
          (and (= (get v x) (get v' y))
               (= (get v y) (get v' x))))))

  (testing "leaves the rest intact"
    (is (let [v [1 2 3 4 5 6]]
          (= (subvec v 2)
             (subvec (#'cipher/swapv v 0 1) 2)))))

  (testing "boundaries"
    (is (let [v [1 2 3 4]]
          (= [4 2 3 1]
             (#'cipher/swapv v 0 3)))))

  (testing "out of boundaries"
    (is (thrown? AssertionError (#'cipher/swapv [1 2] -1 0)))
    (is (thrown? AssertionError (#'cipher/swapv [1 2] 0 2)))))
