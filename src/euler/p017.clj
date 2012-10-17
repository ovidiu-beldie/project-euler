;If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are
;3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many
;letters would be used?

;NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains
;23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when
;writing out numbers is in compliance with British usage.


(def one-19 ["" "one" "two" "three" "four" "five" "six" "seven" "eight"
            "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
            "sixteen" "seventeen" "eighteen" "nineteen"])

(def tens ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])



(defn process-letter-format
  [coll]
  (let [stop (inc (count coll))]
    (->> coll
         (map count)
         (interleave (range stop))
         (apply hash-map))))

(def memo-proc-letter-format (memoize process-letter-format))

(def map-len-1-19 (memo-proc-letter-format one-19))
(def map-len-10s (memo-proc-letter-format tens))


(defn thousands
  "Computes the length of the thousands' part of a numeral expressed in letters.
Here a simplification has been made according to the problem's requirements
(we will only handle the number 1000)"
  [n]
  (if (= n 1000) 11 0))                 ;length of "one thousand" = 11

(defn hundreds
  "Computes the length of the hundreds' part of a numeral expressed in letters"
  ([n] (hundreds n map-len-1-19))
  ([n m]
     (let [hundred-len 7                   ;length of "hundred" = 7
           digit (mod (long (/ n 100)) 10) ;the digit for the "hundreds" position
           and-len (if (zero? (mod n 100)) 0 3)]
       (if (zero? digit)
         0
         (+ (m digit) hundred-len and-len)))))

(defn tens-and-units
  "Computes the length of the tens and units parts of a numeral expressed in letters"
  ([n] (tens-and-units n map-len-1-19 map-len-10s))
  ([n m1 m2]
     (let [xy (mod n 100)]
       (if (< xy 20)
         (m1 xy)
         (+ (m2 (long (/ xy 10)))
            (m1 (mod xy 10)))))))

(defn length-in-letters
  "Computes the length of the number n when expressed in letters"
  [n]
  (+ (thousands n) (hundreds n) (tens-and-units n)))


(defn p17
  "Solution to P17"
  ([] (p17 1 1000))
  ([start stop]
     (reduce + (map length-in-letters (range start (inc stop))))))



