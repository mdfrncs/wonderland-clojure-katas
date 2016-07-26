(ns tiny-maze.solver)


(defn- next?
  "True if x is the next node when backtracking from node; false otherwise"
  [adjacent node distance x]
  (and (= (val x) distance)
       (some #(= % node) (adjacent (key x)))))

(defn- back-track
  "List of nodes from root to target"
  [root target nodes adjacent]
  (loop [path (list target)]
    (let [node (first path)
          distance (dec (nodes node))
          next? (partial next? adjacent node distance)]
      (if (= node root)
        path
        (recur (conj path (key (first (filter next? nodes)))))))))


(defn- update-mapping
  "Returns a new map with an updated distance for 'node' of at most 'from + 1'"
  [from map node]
  (update map node #(if (or (nil? %)
                            (> % from))
                     (inc from)
                     %)))

(defn bfs
  "Breadth-first search: returns map of expanded nodes to distance costs from root. Stops once target is reached"
  [root, target, adjacent]
  (loop [nodes {root 0}
         expand (list root)]
    ;; No solution
    (if (and (empty? expand)
             (not (contains? nodes target)))
      nil
      ;; Solution found
      (if (contains? nodes target)
        (back-track root target nodes adjacent)
        ;; Continue search
        (let [node (first expand)
              adj (adjacent node)
              updated-nodes (reduce (partial update-mapping (nodes node)) nodes adj)
              to-expand (concat (rest expand) (filter (complement (partial contains? nodes)) adj))]
          (recur updated-nodes to-expand))))))

(defn- unflatten
  [width index]
  [(int (/ index width)) (mod index width)])

(defn- reflatten
  [width [y x]]
  (+ (* y width) x))

(defn solve-maze
  [maze]
  (let [height (count maze)
        width (count (first maze))

        flat-maze (vec (flatten maze))

        ;; Extract start and end nodes
        unflatten (partial unflatten width)
        reflatten (partial reflatten width)
        root (unflatten (.indexOf flat-maze :S))
        target (unflatten (.indexOf flat-maze :E))

        ;; Define adjacent function for bfs
        adjacent (fn [[y x]]
                   (vec (filter (fn [[h w]]
                             (cond
                               (> 0 w) false
                               (> 0 h) false
                               (<= width w) false
                               (<= height h) false
                               (= 1 ((maze h) w)) false
                               :else true))
                           [[y (inc x)] [y (dec x)] [(inc y) x] [(dec y) x]])))

        ;; Path through maze
        path (bfs root target adjacent)]

    ;;Format result: flatten both maze and path, assoc :x, re-expand
    (mapv vec
          (partition width
                     (apply assoc flat-maze
                            (mapcat (fn [i] [i :x])
                                    (map reflatten path)))))))