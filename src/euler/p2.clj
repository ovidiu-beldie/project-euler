;Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
;1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

(ns euler.p2)

;Got this one from clojuredocs.org
(defn fibo [a b] (lazy-seq (cons a (fibo b (+ a b)))))

(defn p2 [lim]
  "lim = 4000000"
  (reduce + (filter even? (take-while #(< % lim) (fibo 0 1)))))
