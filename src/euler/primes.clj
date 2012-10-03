(ns euler.primes)

(defn prime?
  "Returns true if n is prime, false otherwise"
  [n]
  (not-any? #(zero? (mod n %)) (range 2 (inc ( Math/sqrt n)))))

(defn next-prime
  "Returns the first prime which is greater than n"
  [n]
  (loop [x (inc n)]
    (if (prime? x) x (recur (inc x)))))

(defn prime-seq
  "Returns a lazy seq of primes"
  []
  (iterate next-prime 2))

(defn nth-prime
  "Returns the nth prime"
  [n]
  (nth (prime-seq) n))

(defn factorize
  "Computes the prime factors of the supplied int. Returns a map with
the prime factors as keys and their respective cardinality as keys"
  [num]
  (let [div? (fn [n d] (zero? (mod n d)))]
    ;the loop extracts the prime factors by repeated divisions with increasing
    ;divisors starting at 2
    (loop [n num, d 2, fs []]
      (cond
       (< n 2) (frequencies fs)
       (div? n d) (recur (/ n d) 2 (conj fs d))
       :else (recur n (inc d) fs)))))