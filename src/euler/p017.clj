;If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are
;3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many
;letters would be used?

;NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains
;23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when
;writing out numbers is in compliance with British usage.


(def one-19 ["one" "two" "three" "four" "five" "six" "seven" "eight"
            "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
            "sixteen" "seventeen" "eighteen" "nineteen"])

(def tens ["twenty" "thirty" "fourty" "fifty" "sixty" "seventy" "eighty" "ninty"])



(defn process-letter-format
  [coll start step]
  (let [stop (+ start (count coll) 1)]
    (->> coll
         (map count)
         (interleave (range start stop step))
         (apply hash-map))))


(defn thousands
  [n]
  (if (= n 1000) 11 0))

(defn hundreds
  [n m]
  (let [hundred-len 7
        mult (m (long (/ n 100)) 0)
        and-len (if (zero? (mod n 100)) 0 3)]
    (if (zero? mult)
      0
      (+ mult hundred-len and-len))))

(defn tens-&-units
  [n m1 m2]
  )




