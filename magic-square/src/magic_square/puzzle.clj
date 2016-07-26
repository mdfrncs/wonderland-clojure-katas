(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

;; Copied from test code
(defn- sum-rows [m]
  (map #(reduce + %) m))

(defn- sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn- sum-diagonals [in]
  (let [m (vec (map #(vec %) in))]
    [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
     (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))]))

(defn- magic-square? [values]
  (and (= (set (sum-rows values))
          (set (sum-cols values))
          (set (sum-diagonals values)))
       (= 1
          (count (set (sum-rows values)))
          (count (set (sum-cols values)))
          (count (set (sum-diagonals values))))))

(defn permutations
  [coll]
  (if (empty? coll)
    ['()]
    (mapcat (fn [x]
              (map #(conj % x)
                   (permutations (remove #(= x %) coll))))
         coll)))

(defn- to-square
  [value]
  (vec (partition 3 value)))

(defn magic-square [values]
  (mapv vec
        (first
          (filter magic-square?
                  (map to-square (permutations values))))))

