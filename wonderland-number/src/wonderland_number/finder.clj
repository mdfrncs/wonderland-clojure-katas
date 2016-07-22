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
