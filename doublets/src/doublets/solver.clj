(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn has-link? [a b]
  (= 1 (count (filter (complement zero?) (map #(- (int %1) (int %2)) a b)))))

(defn adjacent [node]
  (set (filter (partial has-link? node) (filter #(= (count node) (count %)) words))))

(defn back-track [root target nodes]
  (loop [path (list target)]
    (let [node (first path)
          distance (dec (nodes node))]
    (if (= node root)
      path
      (recur (conj path
                   (key (first
                          ;;Get any node which is adjacent to this node which also have distance - 1 (ie, next node in back track)
                       (filter #(and (= (val %) distance)
                                     (contains? (adjacent (key %)) node))
                               nodes)))))))))

(defn update-mapping [from map node]
  (update map node #(if (or (nil? %)
                            (> % from))
                     (inc from)
                     %)))

(defn bfs [root, target]
  (loop [nodes {root 0}
         expand (list root)]
    ;; No solution
    (if (and (empty? expand)
             (not (contains? nodes target)))
      nil
      ;; Solution found
      (if (contains? nodes target)
        (back-track root target nodes)
        ;; Continue search
        (let [node (first expand)
              adj (adjacent node)
              updated-nodes (reduce (partial update-mapping (nodes node)) nodes adj)
              to-expand (concat (rest expand) (filter (complement (partial contains? nodes)) adj))]
          (recur updated-nodes to-expand))))))

(defn doublets [word1 word2]
  (vec (bfs word1 word2)))

