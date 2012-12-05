;The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

;Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

;(Please note that the palindromic number, in either base, may not include leading zeros.)

(defn palindromic?
  "Checks if x is a palindrome"
  [x]
  (let [s (str x)]
    (= s (apply str (reverse s)))))

(defn p36
  "Solution for P36"
  ([] (p36 1000000))
  ([n]
     (->> (range n)
          (filter palindromic?)
          (reduce #(conj % [%2 (Long/toBinaryString %2)]) [])
          (filter (comp palindromic? last))
          (map first)
          (apply +))))