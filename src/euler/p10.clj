;The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;Find the sum of all the primes below two million.

(ns euler.p10
  (:use [euler.p3 :only (primes)]))

;;; Using division testing
(defn sum [lim]
  (reduce + (take-while #(< % lim) (primes))))


;;; Using Eratosthenes's sieve
;; As suggested on the euler-project site, we can optimize
;; the sieving by working only with odd numbers. However, this
;; requires a (simple) formula to compute the new indexes

(defn index-of [n]
  "Returns the index of n in the sieve
  which is [(n+1)/2]-1"
  (if (odd? n)
    (dec (/ (inc n) 2))
    0))  ; the only even prime is 2 and it's the first in the seq

(defn find-next-prime [n s s-count]
  "Find the first prime superior to n in the sieve s"
  (loop [i (inc (index-of n))]
    (if (>= i s-count)
      nil
      (if (val (first (s i)))
        (key (first (s i)))
        (recur (inc i))))))

(defn mark-multiples [n si si-count]
  "Marks all the multiples of n in the sieve si
   which has si-count elements"
  (let [index-n (index-of n)]
    (loop [i (+ index-n n), s si]
      (if (>= i si-count)
        s
        (let [si
                (if (val (first (s i)))
                  (let [marked {(key (first (s i))), false}]
                    (assoc s i marked))
                s)]
          (recur (+ i n), si))))))

(defn sieve [lim]
  "Sieve up to lim (e.g. 2000000)"
  (let [nbs (cons 2 (filter odd? (range 2 (inc lim))))
        si (vec (for [x nbs] {x true}))
        si-count (count si)]
    (loop [s si, old-n 2]
      (let [n (find-next-prime old-n s si-count)]
        (if (nil? n)
          (map #(key (first %)) (filter #(val (first %)) s))
          (recur (mark-multiples n s si-count) n))))))
