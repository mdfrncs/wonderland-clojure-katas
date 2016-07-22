(ns alphabet-cipher.coder)

(defn to-int [c]
  (- (int c) 97))

(defn to-char [i]
  (char (+ (mod i 26) 97)))

(defn encode-char [a b]
  (to-char (+ (to-int a) (to-int b))))

(defn decode-char [a b]
  (to-char (- (to-int a) (to-int b))))

;; Smallest substring of input which when cycled matches input
(defn greedy-match-key [in]
  (let [length (count in)
        key (seq in)]
    (apply str
           (loop [n 1]
             (let [test (take n key)]
               (if (= (take length (cycle test)) key) test (recur (inc n))))))))

(defn encode-decode [keyword message algo]
  (apply str (map algo message (cycle keyword))))

(defn encode [keyword message]
  (encode-decode keyword message encode-char))

(defn decode [keyword message]
  (encode-decode keyword message decode-char))

(defn decipher [cipher message]
  (greedy-match-key (decode message cipher)))

