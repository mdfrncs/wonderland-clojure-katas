(ns wonderland-number.finder-test
  (:require [clojure.test :refer :all]
            [wonderland-number.finder :refer :all]))

(defn hasAllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(deftest test-wonderland-number
  (testing "A wonderland number must have the following things true about it"
    (let [wondernum (wonderland-number)]
      (is (= 6 (count (str wondernum))))
      (is (hasAllTheSameDigits? wondernum (* 2 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 3 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 4 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 5 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 6 wondernum))))))

(deftest test-wonderland-cube-numbers
  (testing "All wonderland cube numbers must be equal to the sum of the cube of their digits"
    (let [wondernums (wonderland-cube-numbers)]
      (every? is-sum-of-cubes-solution? wondernums))))