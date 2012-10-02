;A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;Find the largest palindrome made from the product of two 3-digit numbers.

(use '[euler.palindrome :only (palindrome?)])

(defn p4
  "Solution to P4"
  []
  (let [prods (for [x (range 800 1000) y (range 800 x)] (* x y))]
    (apply max (filter palindrome? prods))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Earlier version (march 2012)

(defn palindrome-1? [n]
  "Tests if n is a palindrome"
  (loop [s (.toString n)]
    (if (< (count s) 2)
      true 
      (if (not= (first s) (last s))
        false
        (recur (drop-last (next s)))))))

(defn find-palindrome [low-lim up-lim]
  "For this problem low-lim = 900, up-lim = 999"
  (let [r (range low-lim up-lim)]
    (apply max (filter palindrome-1? (for [x r, y r :when (< x y)] (* x y))))))
      
