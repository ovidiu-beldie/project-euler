;The sequence of triangle numbers is generated by adding the natural numbers. So the 7th
;triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
;1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;Let us list the factors of the first seven triangle numbers:
;     1: 1
;     3: 1,3
;     6: 1,2,3,6
;    10: 1,2,5,10
;    15: 1,3,5,15
;    21: 1,3,7,21
;    28: 1,2,4,7,14,28
;We can see that 28 is the first triangle number to have over five divisors.
;What is the value of the first triangle number to have over five hundred divisors?

(require '[clojure.contrib.combinatorics :as c])
(use '[euler.primes :only (factorize primes-div)])

(defn count-divisors
  "Counts n's divisors (including 1 and n itself)"
  [n]
  (->> n
       (factorize)
       (reduce #(into % (repeat (val %2) (key %2))) [])
       (c/subsets)
       (map vec)
       (apply sorted-set)
       (count)))

(defn triangle-nums
  "Returns a lazy seq of triangle numbers"
  ([] (triangle-nums 2 1))
  ([i n]
     (cons n (lazy-seq (triangle-nums (inc i) (+ i n))))))

(defn p12
  "Solution to P12"
  ([] (p12 500))
  ([n]
     (first (drop-while #(< (count-divisors %) n) (triangle-nums)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Previous version (march 2012)

(defn factorize-old [n]
  (letfn [(least-fact [n]
            (first (drop-while #(not (zero? (rem n %))) primes-div)))]
    (loop [x n, facts []]
      (let [f (least-fact x)]
        (if (> f x)
          facts
          (if (= f x)
            (conj facts f)
            (recur (/ x f) (conj facts f))))))))

(defn triangle-numbers-1 []
  "Generate infinite seq of triangle numbers"
  (map first (iterate (fn [[n i]] [(+ n (inc i)) (inc i)]) [1 1])))

(defn filter-uniques [coll]
  "Filter unique values (remove doubles)"
  (loop [filtered-c (list), c coll]
    (if (nil? (first c))
      filtered-c
      (let [new-c
                  (if (apply distinct? (first c) filtered-c)
                    (conj filtered-c (first c))
                    filtered-c)]
        (recur new-c, (rest c))))))

(defn divisors-1 [n]
  "Return the list of divisors of n"
  (let [factors (factorize-old n)
        k-seq (range 1 (inc (count factors)))
        divisors-raw (map #(c/combinations factors %) k-seq)
        divs (loop [divs (list), d-raw divisors-raw]
                (if (nil? (first d-raw))
                  divs
                  (recur (apply conj divs (first d-raw)), (rest d-raw))))]
    (filter-uniques divs)))

(defn p12-1 []
  (first (drop-while #(> 500 (inc (count (divisors-1 %)))) (drop 1 (triangle-numbers-1)))))
