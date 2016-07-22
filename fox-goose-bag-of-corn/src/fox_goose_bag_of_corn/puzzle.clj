(ns fox-goose-bag-of-corn.puzzle)

;; Gets the 'from' and 'to' sets being manipulated on this step
(defn from-to-sets [[start boat end] location]
  (case location
    :start [start boat]
    :end [end boat]
    :boat-to-end [boat end]
    :boat-to-start [boat start]))

;; Constructs the next step of the plan from the last step, current location and modified from/to sets
(defn next-state [[start boat end] location [from to]]
  (case location
    :start [[from to end] :boat-to-end]
    :end [[start to from] :boat-to-start]
    :boat-to-end [[start from to] :end]
    :boat-to-start [[to from end] :start]))

(defn game-failed? [state]
    (or (= state #{:fox :goose})
        (= state #{:goose :corn})))

(defn state-valid? [[state _]]
  (and (not-any? game-failed? state)
       (let [[start boat end] state]
         (< (count boat) 4))))

(defn game-won? [[[start boat end] location]]
  (and (= location :end)
       (= start #{})
       (= boat #{:boat})
       (= end #{:fox :goose :corn :you})))

;; Move symbol from 'from' to 'to'
(defn gen-moves-for-symbol [from to symbol]
  [(set (remove #(= % symbol) from)) (conj to symbol)])

;; Generate all valid steps from this state
(defn gen-moves [[state location]]
  (let [[from to] (from-to-sets state location)
        to-with-you (conj to :you)
        from-without-you (remove #(= % :you) from)]
    (filter state-valid?
            (map (partial next-state state location)
                 (conj
                   (map (partial gen-moves-for-symbol from-without-you to-with-you)
                        (filter #(not= % :boat) from-without-you))
                   [(set from-without-you) to-with-you])))))

;; Strip location and convert sets to vectors
(defn convert-soln [[[start boat end] _]]
  [(vec start) (vec boat) (vec end)])

(defn river-crossing-plan
  ([] (let [move [[#{:fox :goose :corn :you} #{:boat} #{}] :start]]
        (vec (map convert-soln (river-crossing-plan move [])))))
  ([move moves]
   (cond
     (game-won? move) (conj moves move)
     (some #(= % move) moves) nil
     :else (first (sort-by count (filter some? (map #(river-crossing-plan % (conj moves move)) (gen-moves move))))))))


