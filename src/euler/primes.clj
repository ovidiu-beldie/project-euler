(ns euler.primes)

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