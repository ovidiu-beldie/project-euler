;The prime factors of 13195 are 5, 7, 13 and 29.
;What is the largest prime factor of the number 600851475143 ?

(ns euler.p3
  (:use [incanter.core :only (sqrt)]))

(defn prime? [n inf-primes]
  "Returns true if n is a prime. 
   inf-primes is the sequence of primes inferior to n"
  (loop [ps inf-primes]
    (let [p (first ps)]
      (if (> p (sqrt n))
        true
        (if (zero? (rem n p)) 
          false
          (recur (next ps)))))))

(defn next-prime [primes]
  "Computes the next prime and adds it to the seq"
  (loop [x (inc (last primes))]
    (if (prime? x primes)
      (conj primes x)
      (recur (inc x)))))
  
(defn primes []
  "Infinite sequence of primes"
  (map last (iterate next-prime [2])))

(defn factorize [n]
  "Computes n's prime factors"
  (letfn [(least-fact [n]
            (first (drop-while #(not (zero? (rem n %))) (primes))))]
    (loop [x n, facts []]
      (let [f (least-fact x)]
        (if (> f x)
          facts
          (if (= f x)
            (conj facts f)
            (recur (/ x f) (conj facts f))))))))
    

      
