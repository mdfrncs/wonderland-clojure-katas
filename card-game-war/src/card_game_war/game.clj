(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn p1-winner?
  [[s1 r1] [s2 r2]]
  (let [rank-compare (- (.indexOf ranks r2) (.indexOf ranks r1))
        suit-compare (- (.indexOf suits s2) (.indexOf suits s1))]
    (> 0 (if (= 0 rank-compare)
      suit-compare
      rank-compare))))

(defn play-round
  [player1-card player2-card]
  (let [c [player1-card player2-card]]
    (if (p1-winner? player1-card player2-card)
      [c []]
      [[] c])))

(defn play-game
  [player1-cards player2-cards]
  (loop [p1 player1-cards
         p2 player2-cards]
    (let [p1-lost (empty? p1)
          p2-lost (empty? p2)
          state [p1-lost p2-lost]]
      (if (or p1-lost p2-lost)
        (vec (map #(if % 0 1) state))
        (let [[p1-change p2-change] (play-round (first p1) (first p2))
              new-p1 (concat (rest p1) p1-change)
              new-p2 (concat (rest p2) p2-change)]
          (recur new-p1 new-p2))))))
