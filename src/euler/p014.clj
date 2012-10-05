;The following iterative sequence is defined for the set of positive integers:

;n → n/2 (n is even)
;n → 3n + 1 (n is odd)

;Using the rule above and starting with 13, we generate the following sequence:
;13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

;It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
;Although it has not been proved yet (Collatz Problem), it is thought that all starting
;knumbers finish at 1.

;Which starting number, under one million, produces the longest chain?

;NOTE: Once the chain starts the terms are allowed to go above one million.


(declare chain)

;;;;;;;;;;;;;;;;;;;;
;; Memoized version. For this version I've split the chain generating function
;; into 2 functions which call each other mutually in order to benefit from the
;; memoization of one of them. This was done after I realized that I couldn't
;; memoize a recursive function (which makes perfect sense).
;; Regarding the performance, I was suprised that the 2 versions perform the same
;; for the first time the function is executed. However if the test is repeated,
;; the advantage of the memoization kicks in and the duration is reduced by around
;; 8 times.
;;
;; As it turns out, my 2 previous attempts are twice as fast as the most recent ondes.
;; But their also less functional.

(defn rest-of-chain
  "Computes the part of the seq that follows n"
  [n coll]
  (let [next-n (if (odd? n) (inc (* n 3)) (/ n 2))]
    (chain next-n coll)))

(def memo-roc (memoize rest-of-chain))

(defn chain
  "Computes the seq starting with n under the assumption that it ends with 1."
  ([n] (chain n []))
  ([n coll]
     (if (= n 1)
       (conj coll 1)
       (conj (memo-roc n coll) n))))

;;;;;;;;;;;;;;;;;;;;
;; Non-memoized version.

(defn chain-1
  "Computes the seq starting with n under the assumption that it ends with 1"
  ([n] (chain-1 n []))
  ([n coll]
     (if (= n 1)
       (conj coll n)
       (let [next-n (if (odd? n) (inc (* 3 n)) (/ n 2))]
         (chain-1 next-n (conj coll n))))))

(defn p14
  "Solution to P14"
  ([] (p14 1000000))
  ([n]
     (let [m (reduce #(conj % [(count (chain %2)) %2]) {} (range 1 n))]
       (m (apply max (keys m))))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Previous attempt (march 2012)

;; First version which I thought would be optimized but it turned out it wasn't

(def chains (ref {}))

(defn comp-chain [number]
  (loop [n number, chain-length 0]
    (if (not (nil? (@chains n)))
      (let [total-chain-length (+ (@chains n) chain-length)]
        ;the rest of the chain is already computed
        (dosync (alter chains assoc number total-chain-length)))
      (if (= 1 n)
        (dosync (alter chains assoc number chain-length))
        (if (even? n)
          (recur (/ n 2) (inc chain-length))
          (recur (inc (* 3 n)) (inc chain-length)))))))

(defn p14-1 [max-val]
  (do
    (dorun (map comp-chain (drop 1 (range (inc max-val)))))
    (prn (first (last (sort-by second @chains))))))

;; Second version which runs a lot faster

(defn chain-length [number]
  (loop [n number, len 0]
    (if (= 1 n)
      len
      (if (even? n)
        (recur (/ n 2) (inc len))
        (recur (inc (* 3 n)) (inc len))))))

(defn p14-2 [max-val]
  (loop [i 1, max-len 0, max-len-val 1]
    (if (= i (inc max-val))
      max-len-val
      (let [l (chain-length i)
            new-max-len (if (> l max-len) l max-len)
            new-max-len-val (if (> l max-len) i max-len-val)]
        (recur (inc i) new-max-len new-max-len-val)))))
