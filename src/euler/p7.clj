;By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;What is the 10 001st prime number?

(ns euler.p7
	(:use [euler.p3 :only (primes)]))

(defn get-prime [n]
	(nth (primes) (dec n)))


