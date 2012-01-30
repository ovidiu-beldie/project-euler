;A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;a^2 + b^2 = c^2
;For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;Find the product abc.

(ns euler.p9
  (:use [clojure.contrib.generic.math-functions :only (sqr)]
        [incanter.core :only (sqrt)]))

(defn squares []
  "Infinite seq of squares"
  (lazy-seq (map sqr (iterate inc 1))))

(defn perf-square? [n]
  "Tests if n is a perfect square"
  (= n (sqr (bigint (sqrt n)))))

(defn pythag-triplets [lim]
  "Computes Pythagorean triplets where both a and b are inferior to lim"
  (let [r (take lim (squares))]
    (for [x r, y r 
        :let [z (+ x y)]
        :when (and (< x y) (perf-square? z))] (map #(long (sqrt %)) [x y z]))))

(defn find-abc [lim sum]
  "lim can be 500, sum = 1000"
  (let [triplets (pythag-triplets lim)
        t (first (filter #(= sum (apply + %)) triplets))]
    (reduce * t)))

