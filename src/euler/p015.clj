;Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking)
;to the bottom right corner.

;How many routes are there through a 20×20 grid?


(defn p15
  "Solution to P15"
  ([] (p15 20))
  ([n]
     (let [fact (fn [x] (apply * (range 2N (inc x))))]
       (bigint (/ (fact (* 2 n)) (Math/pow (fact n) 2))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Previous version

(defn factorials-1[]
  (map second (iterate (fn [[i n]] [(inc i) (* (bigint i) n)]) [1 1])))

; This is a combinatorics / Pascal's triangle problem
(defn p15-1 []
  (let [fact-20 (nth (factorials-1) 20)]
    (/ (nth (factorials-1) 40) fact-20 fact-20)))
