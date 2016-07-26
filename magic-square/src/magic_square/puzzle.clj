(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

;; Copied from test code
(defn sum-rows-prod [m]
  (map #(reduce + %) m))

(defn sum-cols-prod [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sum-diagonals-prod [in]
  (let [m (vec (map #(vec %) in))]
    [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
     (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))]))

(defn magic-square? [values]
  (and (= (set (sum-rows-prod values))
          (set (sum-cols-prod values))
          (set (sum-diagonals-prod values)))
       (= 1
          (count (set (sum-rows-prod values)))
          (count (set (sum-cols-prod values)))
          (count (set (sum-diagonals-prod values))))))

(defn solnspace
  ([values] (mapcat #(solnspace [] values %) values))
  ([array symbols sym]
   (let [to-process (remove #(= % sym) symbols)
         soln (conj array sym)]
     (if (= 2 (count to-process))
       (solnspace soln to-process (first to-process) (last to-process))
       (mapcat #(solnspace soln to-process %) to-process))
     ))
  ([array _ a b]
   (list (conj array a b) (conj array b a))))

(defn magic-square [values]
  (vec (map vec (first
    (filter magic-square?
            (map #(vec (partition 3 %)) (solnspace values)))))))

