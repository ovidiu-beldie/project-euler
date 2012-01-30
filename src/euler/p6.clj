;The sum of the squares of the first ten natural numbers is,
;1^2 + 2^2 + ... + 10^2 = 385
;The square of the sum of the first ten natural numbers is,
;(1 + 2 + ... + 10)^2 = 552 = 3025
;Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(ns euler.p6
  (:use [clojure.contrib.generic.math-functions :only (sqr)]))

(defn diff [lim]
  "lim = 100"
  (let [coll (range (inc lim))
        sum-squares (reduce + (map sqr coll))
        square-sum (sqr (reduce + coll))]
    (- square-sum sum-squares)))
