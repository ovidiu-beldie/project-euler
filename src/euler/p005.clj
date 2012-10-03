;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without
;any remainder.
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(use '[euler.primes :only (factorize)])
(use '[euler.p003 :only (factorize-p3)])
(use '[incanter.core :only (pow)])

(defn p5
  "Solution to P5"
  ([] (p5 20))
  ([lim]
     (let [fact-maps (map factorize (range 1 (inc lim)))
           lcm-facts (apply merge-with max fact-maps)
           r-fn (fn [m kv]
                     (* m (Math/pow (first kv) (second kv))))]
       (long (reduce r-fn 1 (seq lcm-facts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Earlier solution (march 2012)

(defn p5-1 [coll]
  "For this problem coll = (range 20)"
  (let [facts (map factorize-p3 (drop-while #(< % 2) coll))  ; factorize seq members
        histo (map frequencies facts)  ; compute primes' histogram
        highest-order-facts (apply merge-with max histo)] ; get list of primes with max exponent
    (long (reduce * (map #(pow (key %) (val %)) highest-order-facts)))))
