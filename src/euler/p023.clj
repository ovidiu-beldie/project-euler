;A perfect number is a number for which the sum of its proper divisors is exactly equal to the
;number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
;which means that 28 is a perfect number.

;A number n is called deficient if the sum of its proper divisors is less than n and it is called
;abundant if this sum exceeds n.

;As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be
;written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that
;all integers greater than 28123 can be written as the sum of two abundant numbers. However, this
;upper limit cannot be reduced any further by analysis even though it is known that the greatest
;number that cannot be expressed as the sum of two abundant numbers is less than this limit.

;Find the sum of all the positive integers which cannot be written as the sum of two abundant
;numbers.

(use '[euler.primes :only (memo-factorize)])
(require '[clojure.contrib.combinatorics :as c])

(defn proper-divisors
  "Returns all divisors of n except n itself.
Something similar was done for P12 but it's good exercise to do it again."
  [n]
  (->> n
       (memo-factorize)
       (reduce #(into %1 (repeat (val %2) (key %2))) [])
       (c/subsets)
       (distinct)
       (map #(reduce * %))
       (remove #(= n %))))

(defn abundant?
  "Returns true if n is abundant"
  [n]
  (> (reduce + (proper-divisors n)) n))

(defn diff-sorted-ints
  "Removes unwanted from all. The 2 colls must be sorted sets of ints."
  [all unwanted]
  (loop [res [], xs all, us unwanted]
    (if (nil? xs)
      res
      (cond
       (= (first xs) (first us)) (recur res (next xs) (next us))
       :else (recur (conj res (first xs)) (next xs) us)))))

(defn p23
  "Solution for P23"
  ([] (p23 28123))
  ([n]
     (let [ints (range 1 n)
           abund-ns (filter abundant? ints)
           abund-sums (for [x abund-ns, y (take-while #(<= % x) abund-ns)
                               :let [s (+ x y)] :when (< s n)] s)]
       (->> abund-sums
            (apply sorted-set)
            (diff-sorted-ints ints)
            (reduce +)))))




