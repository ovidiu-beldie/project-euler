(ns euler.fibo)

(defn fibo
  "Returns a lazy seq of the Fibonacci numbers"
  []
  (let [fib (fn [[a b]] [b (+ a b)])]
    (map first (iterate fib [0 1]))))

(defn fibo-1
  "Returns a lazy seq of the Fibonacci numbers"
  ([] (fibo-1 0 1))
  ([a b] (lazy-seq (cons a (fibo-1 b (+ a b))))))
