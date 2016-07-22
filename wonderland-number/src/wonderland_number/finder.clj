(ns wonderland-number.finder)

;;Copied from test code, but fixed since that version fails on inputs like [112 122]
(defn same-digits? [n1 n2]
  (let [s1 (sort (str n1))
        s2 (sort (str n2))]
    (= s1 s2)))

(defn is-solution? [number]
  (every? #(same-digits? number (* number %)) (range 2 6)))

(defn wonderland-number []
  (first (filter is-solution? (range 100000 1000000))))

;;Day 4 exercise: numbers under 1000 which are equal to the sum of the cubes of their digits
(defn is-sum-of-cubes-solution? [number]
  (= number (reduce + (map #(let [i (Character/digit % 10)] (* i i i)) (str number)))))

;;Day 4 exercise: numbers under 1000 which are equal to the sum of the cubes of their digits
(defn wonderland-cube-numbers []
  (filter is-sum-of-cubes-solution? (range 1000)))