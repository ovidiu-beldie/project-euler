;By starting at the top of the triangle below and moving to adjacent
;numbers on the row below, the maximum total from top to bottom is 23.

;   3
;  7 4
; 2 4 6
;8 5 9 3

;That is, 3 + 7 + 4 + 9 = 23.

;Find the maximum total from top to bottom of the triangle below:

(require '[clojure.string :as str])

(def triangle
"                    75
                   95 64
                  17 47 82
                18 35 87 10
               20 04 82 47 65
             19 01 23 75 03 34
            88 02 77 73 07 63 67
          99 65 04 28 06 16 70 92
         41 41 26 56 83 40 80 70 33
       41 48 72 33 47 32 37 16 94 29
      53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
   91 71 52 38 17 14 91 43 58 50 27 29 48
 63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")

(defn format-triangle
  ([] (format-triangle triangle))
  ([tr]
     (let [format-row (fn [row]
                        (->> row
                             (partition 2)
                             (map str/join)
                             (map #(Integer/valueOf %))))]
       (->> tr
            (remove #{\space})
            (str/join)
            (str/split-lines)
            (map format-row)
            (map vec)
            ))))

(defn reducer
  [r1 r2]
  (let [r2-indexed (->> r2
                        (interleave (iterate inc 0))
                        (partition 2)
                        (map vec))
        add-max (fn [coll [i v]]
                  (conj coll (+ v (max (get r1 (dec i) 0) (get r1 i 0)))))]
    (reduce add-max [] r2-indexed)))

(defn p18
  ([] (p18 triangle))
  ([t]
     (->> t
          (format-triangle)
          (reduce reducer)
          (apply max))))