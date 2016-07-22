(ns fox-goose-bag-of-corn.puzzle-test
  (:require [clojure.test :refer :all]
            [fox-goose-bag-of-corn.puzzle :refer :all]
            [clojure.set]))

(defn cross-product [sets symbols]
  (for [set sets,
        symbol symbols]
    (conj set symbol)))

;; Generate all possible game states
(defn generate-states
  ([] (generate-states '(:fox :goose :corn :you :boat)))
  ([symbols] (map vec (keys (group-by sort (generate-states symbols #{#{}} 0)))))
  ([symbols sets size]
   (if (> size (count symbols))
     '()
     (concat sets (generate-states symbols (cross-product sets symbols) (inc size))))))

(defn validate-move [step1 step2]
  (testing "only you and another thing can move"
    (let [diff1 (clojure.set/difference step1 step2)
          diff2 (clojure.set/difference step2 step1)
          diffs (concat diff1 diff2)
          diff-num (count diffs)]
      (is (> 3 diff-num))
      (when (pos? diff-num)
        (is (contains? (set diffs) :you)))
      step2)))

(deftest test-river-crossing-plan
  (let [crossing-plan (map (partial map set) (river-crossing-plan))]
    (testing "you begin with the fox, goose and corn on one side of the river"
      (is (= [#{:you :fox :goose :corn} #{:boat} #{}]
             (first crossing-plan))))
    (testing "you end with the fox, goose and corn on one side of the river"
      (is (= [#{} #{:boat} #{:you :fox :goose :corn}]
             (last crossing-plan))))
    (testing "things are safe"
      (let [left-bank (map first crossing-plan)
            right-bank (map last crossing-plan)]
        (testing "the fox and the goose should never be left alone together"
          (is (empty?
               (filter #(= % #{:fox :goose}) (concat left-bank right-bank)))))
        (testing "the goose and the corn should never be left alone together"
          (is (empty?
               (filter #(= % #{:goose :corn}) (concat left-bank right-bank)))))))
    (testing "The boat can carry only you plus one other"
      (let [boat-positions (map second crossing-plan)]
        (is (empty?
             (filter #(> (count %) 3) boat-positions)))))
    (testing "moves are valid"
      (let [left-moves (map first crossing-plan)
            middle-moves (map second crossing-plan)
            right-moves (map last crossing-plan)]
        (reduce validate-move left-moves)
        (reduce validate-move middle-moves)
        (reduce validate-move right-moves)))))

(deftest test-game-failed?
  (testing "[:goose :corn] is a game failed condition"
    (is (true? (game-failed? #{:goose :corn}))))
  (testing "[:goose :fox] is a game failed condition"
    (is (true? (game-failed? #{:goose :fox}))))
  ; All other states are valid
  (let [states (filter #(not (or (= #{:goose :corn} (set %))
                                 (= #{:goose :fox} (set %)))) (generate-states))]
    (doseq [state states]
      (testing (str state " is not a game failing condition")
        (is (not (true? (game-failed? (set state)))))))))

(deftest test-game-won?
  (testing "[] [:boat] [:you :goose :fox :corn] is a game won condition"
    (is (true? (game-won? [[#{} #{:boat} #{:goose :you :fox :corn}] :end])))))

(deftest test-from-to-sets
  (let [start [:a :b]
        boat [:c]
        end [:d :e :f]
        state [start boat end]]
    (testing ":start returns first two vectors"
      (is (= [start boat] (from-to-sets state :start))))
    (testing ":end returns end and boat vectors"
      (is (= [end boat] (from-to-sets state :end))))
    (testing ":boat-to-end returns boat and end vectors"
      (is (= [boat end] (from-to-sets state :boat-to-end))))
    (testing ":boat-to-start returns boat and start vectors"
      (is (= [boat start] (from-to-sets state :boat-to-start))))))

