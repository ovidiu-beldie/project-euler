;It can be seen that the number, 125874, and its double, 251748, contain exactly the same
;digits, but in a different order.
;Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same
;digits.

(defn digits
  "Returns a sorted seq of n's digits"
  [n]
  (->> n (str) (seq) (map str) (sort)))

(defn firstst-digit-1?
  [n]
  (->> n (str) (first) (= \1)))

(defn p52
  "Solution to P52"
 []
 (let [same-digits? (fn [x y]
                      (= (digits x) (digits y)))
       ints (iterate inc 1)]
   (first (for [x ints :let [sd? (partial same-digits? x)]
                :when (and (first-digit-1? x)
                           (sd? (* x 2))
                           (sd? (* x 3))
                           (sd? (* x 4))
                           (sd? (* x 5))
                           (sd? (* x 6)))]
            x))))
