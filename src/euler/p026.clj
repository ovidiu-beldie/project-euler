;A unit fraction contains 1 in the numerator. The decimal representation of the unit
;fractions with denominators 2 to 10 are given:

;    1/2	= 	0.5
;    1/3	= 	0.(3)
;    1/4	= 	0.25
;    1/5	= 	0.2
;    1/6	= 	0.1(6)
;    1/7	= 	0.(142857)
;    1/8	= 	0.125
;    1/9	= 	0.(1)
;    1/10	= 	0.1

;Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7
;has a 6-digit recurring cycle.

;Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal
;fraction part.

(require '[clojure.string :as str])

(defn flc
  "Finds the longest recurring cycle for divisor d"
  ([d] (flc d 50))
  ([d p]
     (let [s (->> (/ 1M d)
                  (with-precision p :rounding FLOOR)
                  (str) (drop 2) (reverse) (str/join))]
       (if (< (count s) p)
         0                              ;no cycle
         (loop [n 1]
           (let [half-p (/ p 2)
                 cycle-seq (take half-p (cycle (take n s))) ;make seq by repeating first n chars
                 s-seq (take half-p s)]
             (cond
              (= cycle-seq s-seq) n
              (> n (/ p 4)) (flc d (* p 5))
              :else (recur (inc n)))))))))

(defn p26
  "Solution to P26"
  ([] (p26 2 1000))
  ([start end]
     (let [ red (fn [max curr]
                  (if (> (last curr) (last max)) curr max))           
           v (reduce #(conj % [%2 (flc %2)]) [] (range start end))]
       (first (reduce red v)))))