(ns euler.primes)

(use '[clojure.set :only (difference)])

(defn prime?
  "Returns true if n is prime, false otherwise"
  [n]
  (cond
   (< n 2) false
   (= n 2) true
   (zero? (mod n 2)) false
   :else (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n))))))

(defn next-prime
  "Returns the first prime which is greater than n"
  [n]
  (loop [x (inc n)]
    (if (prime? x) x (recur (inc x)))))

(defn- primes-by-divs
  "Returns a lazy seq of primes"
  []
  (iterate next-prime 2))

(defn factorize
  "Computes the prime factors of the supplied int. Returns a map with
the prime factors as keys and their respective cardinality as vals"
  [num]
   ;the loop extracts the prime factors by repeated divisions with increasing
   ;divisors starting at 2
  (loop [n num, d 2, fs []]
    (cond
     (< n 2) (frequencies fs)
     (zero? (mod n d)) (recur (/ n d) 2 (conj fs d))
     :else (recur n (inc d) fs))))

(declare m-fact)

(defn fact
  "Computes the argument's prime factors. Recursive by calling a memoized version
of itself."
  [num]
  (if (< num 2)
    []
    (loop [d 2]
      (cond
       (zero? (mod num d)) (cons d (m-fact (/ num d)))
       :else (recur (inc d))))))

(def m-fact (memoize fact))

(defn memo-factorize
  "Computes the prime factors of the supplied int. Returns a map with
the prime factors as keys and their respective cardinality as keys.
Uses a memoized function to compute the prime factors."
  [num]
  (frequencies (m-fact num)))

(def primes-div (primes-by-divs))

;;; Sieve of Eratosthenes

(defn- zero-multiples
  "Sets to 0 the vector's values with indexes that are
i's multiples"
  [vect i]
  (let [s (count vect)]
    (loop [j (* 2 i), v vect]
      (cond
       (>= j s) v
       (zero? (v j)) (recur (+ j i) v)    ;don't assoc if already 0
       :else (recur (+ j i) (assoc v j 0))))))

(defn primes-sieve
  "Returns primes lower than lim. Not lazy.
We use the whole range form 0 to lim to take advantage of the index being equal
to the value. However we build this range with the evens preset to 0. We explore
only odds and set their mutiples to 0. In the end we remove the zero values"
  [lim]
  (let [odds (range 1 lim 2)            ;seq of odds smaller than lim
        nums (vec (interleave (repeat 0) odds))] ;build complete range with 0's in place of evens
    (loop [i 3, c nums]                 ;explore only odds
      (cond
       (>= i lim) (->> c                ;we're done
                       (remove zero?)   ;remove 0's (evens and multiples)
                       (drop 1)         ;remove 1
                       (cons 2))        ;add 2
       (or (even? i) (zero? (num i))) (recur (+ i 2) c)
       :else (recur (+ i 2) (zero-multiples c i))))))

;;; A different approach

(defn primes-sieve-sets
  "Returns primes lower than lim. Not lazy. Fun(ctional) but very slow."
  [lim]
  (let [nums (->> lim               ;make a set of odd ints lower than lim (don't forget 2!)
               (range 3)
               (filter odd?)
               (apply sorted-set))]
    (loop [i 0, xs nums]
      (if (>= i (count xs))
        (cons 2 xs)
        (let [x (nth (seq xs) i)          ;x is the i-th element in the set
              ms (apply sorted-set (range (* 2 x) lim x))]
          (recur (inc i) (difference xs ms)))))))