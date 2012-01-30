;A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;Find the largest palindrome made from the product of two 3-digit numbers.

(ns euler.p4)

(defn palindrom? [n]
  (loop [s (.toString n)]
    (if (< (count s) 2)
      true 
      (if (not= (first s) (last s))
        false
        (recur (drop-last (next s)))))))

(defn find-palindrom [low-lim up-lim]
  (let [r (range low-lim up-lim)]
    (apply max (filter palindrom? (for [x r, y r :when (< x y)] (* x y))))))
      
