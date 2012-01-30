;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(ns euler.p5
  (:use [euler.p3 :only (factorize)]
        [incanter.core :only (pow)]))

(defn lcd [coll]
  (let [facts (map factorize (drop-while #(< % 2) coll))
        histo (map frequencies facts)
        highest-order-facts (apply merge-with max histo)]
    (long (reduce * (map #(pow (key %) (val %)) highest-order-facts)))))
