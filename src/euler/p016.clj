;215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

;What is the sum of the digits of the number 2^1000?

(use '[clojure.contrib.math :only (expt)])

(defn p16
  "Solution to P16"
  ([] (p16 1000))
  ([n]
     (->> (expt 2M n)
          (str)
          (seq)
          (map (comp #(Integer/valueOf %) str))
          (apply +))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Previous solution (march 2012)

(defn p16-1 []
  (let [n (expt (bigdec 2) 1000)
        n-str (.toString n)
        digits-str (map str (vec n-str))
        digits (map #(Integer/valueOf %) digits-str)]
    (prn (reduce + digits))))
