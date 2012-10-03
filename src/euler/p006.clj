;The sum of the squares of the first ten natural numbers is,
;1^2 + 2^2 + ... + 10^2 = 385
;The square of the sum of the first ten natural numbers is,
;(1 + 2 + ... + 10)^2 = 552 = 3025
;Hence the difference between the sum of the squares of the first ten natural numbers and the
;square of the sum is 3025 − 385 = 2640.
;Find the difference between the sum of the squares of the first one hundred natural numbers
;and the square of the sum.

(defn p6
  "Solution to P6"
  ([] (p6 100))
  ([lim]
     (let [r (range 1 (inc lim))
           sqr (fn [x] (* x x))]
       (- (sqr (reduce + r))
          (reduce + (map sqr r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Earlier solution

(defn diff [lim]
  "lim = 100"
  (let [sqr (fn [x] (* x x))
        coll (range (inc lim))
        sum-squares (reduce + (map sqr coll))
        square-sum (sqr (reduce + coll))]
    (- square-sum sum-squares)))
