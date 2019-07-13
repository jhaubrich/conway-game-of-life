(ns conway-game-of-life.core
  (:gen-class))


(defn empty-board
  "Creates emty board of height and width"
  [h w]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coords"
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))


(defn neighbors
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbors
  [board loc]
  (count (filter #(get-in board %) (neighbors loc))))

(defn indexed-step
  "yields the next state of the board"
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board 
           x 0 
           y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
        (let [new-liveness
              (case (count-neighbors board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Conways Game of Life")
  ;(pprint (nth (iterate indexed-step glider) 8))
  (->
   (iterate indexed-step glider)
   (nth 8)
   pprint))

