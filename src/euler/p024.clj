;A permutation is an ordered arrangement of objects. For example, 3124 is one possible
;permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically
;or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and
;2 are:

;012   021   102   120   201   210

;What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

(require '[clojure.contrib.combinatorics :as c])

(defn p24
  "Solution to P24"
  ([] (p24 1e6))
  ([n]
     (->> (range 10)
          (c/lex-permutations)
          (drop (dec n))
          (first)
          (apply str))))