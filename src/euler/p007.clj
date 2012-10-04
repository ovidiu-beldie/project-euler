;By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime
;is 13.
;What is the 10 001st prime number?

(use '[euler.primes :only (primes-div)])
(use '[euler.p003 :only (primes-p3)])

(defn p7
  "Solution to P7"
  ([] (p7 10001))
  ([n] (nth primes-div (dec n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Earlier solution

(defn get-prime [n]
  (nth (primes-p3) (dec n)))


