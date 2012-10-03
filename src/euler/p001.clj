;If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
;The sum of these multiples is 23.
;Find the sum of all the multiples of 3 or 5 below 1000.

(defn p1
  ([] (p1 3 5 1000))
  ([x y lim]
     (reduce + (into (apply hash-set (range 3 1000 3)) (apply hash-set (range 5 1000 5))))))

(defn p1-1 [x y lim]
  "For this particular problem x=3, y=5, lim=1000"
  (let [make-seq (fn [a] (take-while #(< % lim) (iterate #(+ % a) a)))
        not-multiple? (fn [a b] (not (zero? (rem a b))))]
    (+ (reduce + (make-seq x)) 
       (reduce + (filter #(not-multiple? % x) (make-seq y))))))
