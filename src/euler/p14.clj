;The following iterative sequence is defined for the set of positive integers:

;n → n/2 (n is even)
;n → 3n + 1 (n is odd)

;Using the rule above and starting with 13, we generate the following sequence:
;13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

;It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

;Which starting number, under one million, produces the longest chain?

;NOTE: Once the chain starts the terms are allowed to go above one million.

(ns euler.p14)

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
