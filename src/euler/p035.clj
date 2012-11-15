;The number, 197, is called a circular prime because all rotations of the digits: 197, 971,
;and 719, are themselves prime.

;There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;How many circular primes are there below one million?

(require '[euler.primes :as pr])

(defn rotations
  "Returns all rotations for the number n"
  [n]
  (let [s (str n)
        l (count s)
        r (range 1 l)
        f (fn [i] (Long. (apply str (take l (drop i (cycle s))))))]
    (if (= l 1)
      (list n)
      (map f r))))

(defn p35
  "Solution to P35"
  ([] (p35 1000000))
  ([lim]
     (letfn [(all-rotations-primes? [n]
               (->> (rotations n)
                    (remove pr/prime?)
                    (count) (zero?)))]
       (count (filter all-rotations-primes? (pr/primes-sieve lim))))))

