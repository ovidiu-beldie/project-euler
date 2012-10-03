;A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;a^2 + b^2 = c^2
;For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;Find the product abc.

(defn- sqr [x] (* x x))

;The simplified and optimized version
(defn p9
  ([] (p9 1000))
  ([sum]
     (first (for [b (range 500) a (range b) :let [c (- 1000 a b)]
                  :when (= (sqr c) (+ (sqr a) (sqr b)))] (* a b c)))))

;My first attempt to re-implement this problem; long and poor performance
(defn p9-1
  ([] (p9-1 1000))
  ([sum]
     (let [lim (long (/ sum 3))
           py-triplet? (fn [[ a b c]]
                         (= (sqr c) (+ (sqr a) (sqr b))))
           triplets (for [c (range lim sum), b (range 1 c), a (range 1 b)
                          :when (= sum (+ a b c))] [a b c])]
       (->> triplets
            (drop-while (complement py-triplet?))
            (first)
            (reduce *)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Earlier solution (march 2012)

(defn squares []
  "Infinite seq of squares"
  (lazy-seq (map sqr (iterate inc 1))))

(defn perf-square? [n]
  "Tests if n is a perfect square"
  (= n (sqr (bigint (Math/sqrt n)))))

(defn pythag-triplets [lim]
  "Computes Pythagorean triplets where both a and b are inferior to lim"
  (let [r (take lim (squares))]
    (for [x r, y r 
        :let [z (+ x y)]
        :when (and (< x y) (perf-square? z))] (map #(long (Math/sqrt %)) [x y z]))))

(defn find-abc [lim sum]
  "lim can be 500, sum = 1000"
  (let [triplets (pythag-triplets lim)
        t (first (filter #(= sum (apply + %)) triplets))]
    (reduce * t)))

