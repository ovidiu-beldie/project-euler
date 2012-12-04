;Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly
;into n). If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of
;a and b are called amicable numbers.

;For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
;therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142 ; so d(284) = 220.

;Evaluate the sum of all the amicable numbers under 10000.

(require '[euler.primes :as pr])
(require '[clojure.contrib.combinatorics :as c])

(defn sum-divisors
  "Returns the sum of the proper divisors of n"
  [n]
  (->> n
       (pr/fact) (c/subsets) (map (partial reduce *)) (distinct) (butlast) (reduce +)))

(defn p21
  "Solution for P21"
  ([] (p21 10000))
  ([lim]
     (let [r (fn [m n] (conj m [n (sum-divisors n)]))
           as (reduce r (sorted-map) (range lim))
           f (fn [[x y]]
                (and (= (as y) x)
                     (not= x y)))]
       (->> as
            (filter f)
            (map first)
            (reduce +)))))

;;; Earlier implementation (march 2012)


(defn sum-divisors-old [n]
  (reduce + (filter (fn [x] (zero? (rem n x))) (range 1 (inc (/ n 2))))))

(defn p21-old [max-val]
  (let [s (range 1 (inc max-val))
        s-sum-divs (map sum-divisors s)
        m (zipmap s s-sum-divs)
        pred-1 (fn [e] (= (key e) (m (val e))))
        pred-2 (fn [e] (not (= (key e) (val e))))
        f-m (filter pred-2 (filter pred-1 m))]
    (reduce + (keys f-m))))
        

