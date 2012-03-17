;The following iterative sequence is defined for the set of positive integers:

;n → n/2 (n is even)
;n → 3n + 1 (n is odd)

;Using the rule above and starting with 13, we generate the following sequence:
;13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

;It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

;Which starting number, under one million, produces the longest chain?

;NOTE: Once the chain starts the terms are allowed to go above one million.

(ns euler.p14)

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
    
(defn p14 [max-val]
  (do 
    (dorun (map comp-chain (drop 1 (range (inc max-val)))))
    (prn (first (last (sort-by second @chains))))))
    
    


