;n! means n × (n − 1) × ... × 3 × 2 × 1

;For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
;and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

;Find the sum of the digits in the number 100!


(defn p20
  "Solution to P20"
  ([] (p20 100))
  ([n]
     (->> n
          (inc) (range 1N) (reduce *) (str) (vec)
          (map str) (map #(Long. %)) (reduce +))))


;;; Previous solution (march 2012)

(defn p20-old []
  (let [n (reduce * (range (bigdec 1) 101))
        n-str (.toString n)
        digits-str (map str (vec n-str))
        digits (map #(Integer/valueOf %) digits-str)]
    (prn (reduce + digits))))
