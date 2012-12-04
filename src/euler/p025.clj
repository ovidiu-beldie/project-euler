;The Fibonacci sequence is defined by the recurrence relation:

;    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

;Hence the first 12 terms will be:

;    F1 = 1
;    F2 = 1
;    F3 = 2
;    F4 = 3
;    F5 = 5
;    F6 = 8
;    F7 = 13
;    F8 = 21
;    F9 = 34
;    F10 = 55
;    F11 = 89
;    F12 = 144

;The 12th term, F12, is the first term to contain three digits.

;What is the first term in the Fibonacci sequence to contain 1000 digits?

(require '[euler.fibo :as f])

(defn p25
  "Solution to P25"
  ([] (p25 1000))
  ([n]
     (->> (map vector (range) (f/fibo))
          (drop-while #(< (count (str (last %))) n))
          (ffirst))))


;;; Earlier implementation (march 2012)

(defn fibo-old []
  ; Christophe Grand's solution
  (map first (iterate (fn [[a b]] [(bigdec b) (+ a b)]) [0 1])))

(defn p25-old [len]
  (let [index-fibo (map vector (fibo-old) (range))
        elem-before (last (take-while #(< (count (.toString (first %))) len) index-fibo))]
    (prn (inc (second elem-before)))))

