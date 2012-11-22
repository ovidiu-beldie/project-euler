;In England the currency is made up of pound, £, and pence, p, and there are eight coins in
;general circulation:

;    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

;It is possible to make £2 in the following way:

;    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

;How many different ways can £2 be made using any number of coins?

(def coin-types [1 2 5 10 20 50 100 200])

(defn dec-coin-count
  "Decrements the count for the highest coin type with a non-zero count"
  [nb-coins]
  (if (empty? (remove zero? nb-coins))
    nb-coins
    (let [c (->> nb-coins
                 (reverse)
                 (drop-while zero?))
          c-upd (update-in (vec c) [0] dec)
          nb-zeros (- (count nb-coins) (count c-upd))]
      (into (repeat nb-zeros 0) c-upd))))

(defn make-sum
  "Find all the ways to pay sum using the given set of coins"
  [sum coins]
  (if (= 1 (last coins))
    1
    (let [mvc (last coins)              ;most valuable coin
          lvcs (butlast coins)          ;least valuable coins
          r (range (inc (/ sum mvc)))
          red (fn [nb-solutions nb-mvc]
                (let [rest (- sum (* mvc nb-mvc))]
                  (cond
                   (zero? rest) (inc nb-solutions)
                   (neg? rest) nb-solutions
                   :else (+ nb-solutions (make-sum rest lvcs)))))]
      (reduce red 0 r))))

(defn p31
  "Solution to P31"
  ([] (p31 200))
  ([sum]
     (make-sum sum coin-types)))

