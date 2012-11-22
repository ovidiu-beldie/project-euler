;Surprisingly there are only three numbers that can be written as the sum of fourth powers of
;their digits:

;    1634 = 1^4 + 6^4 + 3^4 + 4^4
;    8208 = 8^4 + 2^4 + 0^4 + 8^4
;    9474 = 9^4 + 4^4 + 7^4 + 4^4

;As 1 = 1^4 is not a sum it is not included.

;The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

(defn sum-of-pows
  "Returns the sum of n's digits raised to the power of p "
  [p n]
  (->> (str n)
       (partition 1) (flatten) (map str) (map #(Long. %))
       (map #(Math/pow % p))
       (reduce +)))

(defn eq-to-sum-pows?
  "Returns true if the sum of n's digits raised to the power of
p is equal to n"
  [p n]
  (if (< n 10)
    false
    (== n (sum-of-pows p n))))

(defn p30
  "Solution for P30"
  ([] (p30 5))
  ([p]
     (->> (range 1e6)
          (filter (partial eq-to-sum-pows? p))
          (reduce +))))

