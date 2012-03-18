;Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.

;How many routes are there through a 20×20 grid?


(ns euler.p15)

(defn factorials[]
  (map second (iterate (fn [[i n]] [(inc i) (* (bigint i) n)]) [1 1])))

; This is a ombinatorics / Pascal's traiangle problem
(defn p15 []
  (let [fact-20 (nth (factorials) 20)]
    (/ (nth (factorials) 40) fact-20 fact-20)))
        

