;The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

;Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

(ns euler.p48
  (:use [clojure.contrib.math :only (expt)]))

(defn p48 [max-val]
  (let [exp (fn [x] (expt (bigdec x) x))
        s (map exp (range 1 (inc max-val)))
        sum (reduce + s)]
    (prn (mod sum (exp 10)))))
        
