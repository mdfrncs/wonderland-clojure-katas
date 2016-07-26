(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (let [one [:spade :one]
        two [:spade :two]
        jack [:spade :jack]
        queen [:spade :queen]
        king [:spade :king]
        ace [:spade :ace]
        three-clubs [:club 3]
        three-spades [:spade 3]
        three-hearts [:heart 3]
        three-diamonds [:diamond 3]]
    (testing "the highest rank wins the cards in the round"
      (is (= [[] [one two]] (play-round one two))))
    (testing "queens are higher rank than jacks"
      (is (= [[queen jack] []] (play-round queen jack))))
    (testing "kings are higher rank than queens"
      (is (= [[] [queen king]] (play-round queen king))))
    (testing "aces are higher rank than kings"
      (is (= [[ace king] []] (play-round ace king))))
    (testing "if the ranks are equal, clubs beat spades"
      (is (= [[three-clubs three-spades] []] (play-round three-clubs three-spades))))
    (testing "if the ranks are equal, diamonds beat clubs"
      (is (= [[] [three-clubs three-diamonds]] (play-round three-clubs three-diamonds))))
    (testing "if the ranks are equal, hearts beat diamonds"
      (is (= [[three-hearts three-diamonds] []] (play-round three-hearts three-diamonds))))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= [0 1] (play-game [[:spade 2] [:heart 7]] [[:spade 3] [:diamond :ace]])))))

