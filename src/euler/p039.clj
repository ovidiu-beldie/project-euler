;If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there
;are exactly three solutions for p = 120.

;{20,48,52}, {24,45,51}, {30,40,50}

;For which value of p ≤ 1000, is the number of solutions maximised?

(defn sqr [x] (* x x))

(defn pyth?
  "Returns true is a²+b²=c²"
  [a b c]
  (= (sqr c) (+ (sqr a) (sqr b))))

(defn p39
  "Solution for P39"
  ([] (p39 1000))
  ([lim]
     (let [sides (for [a (range 1 (/ lim 3)) b (range a (/ lim 2)) c (range b lim)
                       :when (pyth? a b c)] [a b c])
           red (fn [[hp hc] [p c]]
                 (if (> c hc) [p c] [hp hc]))]
       (->> sides
            (map (partial reduce +))
            (frequencies) (reduce red [0 0]) (first)))))
