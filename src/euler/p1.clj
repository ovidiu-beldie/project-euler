;If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;Find the sum of all the multiples of 3 or 5 below 1000.

(ns euler.p1)

(defn p1 [x y lim]
  (let [make-seq (fn [a] (take-while #(< % lim) (iterate #(+ % a) a)))
        not-multiple? (fn [a b] (not (zero? (rem a b))))]
    (+ (reduce + (make-seq x)) 
       (reduce + (filter #(not-multiple? % x) (make-seq y))))))
