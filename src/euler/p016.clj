;215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

;What is the sum of the digits of the number 2^1000?

(ns euler.p16
  (:use [clojure.contrib.math :only (expt)]))

(defn p16-1 []
  (let [n (expt (bigdec 2) 1000)
        n-str (.toString n)
        digits-str (map str (vec n-str))
        digits (map #(Integer/valueOf %) digits-str)]
    (prn (reduce + digits))))
        

