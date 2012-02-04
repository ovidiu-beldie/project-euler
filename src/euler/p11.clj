;In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

;The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

;What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20×20 grid?

(ns euler.p11
  (:use [clojure.contrib.generic.math-functions :only (sqr)]))

(def matrix-as-str 
 "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
  32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
  67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
  24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
  21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(defn make-row [r]
  "Creates a vector of ints from a list of seqs containing the int digits"
  (let [make-int-from-seq (fn [s] (Integer/valueOf (apply str s)))]
  (map make-int-from-seq (partition 2 3 (.trim (apply str r))))))

(defn build-matrix [m-str]
  "Build the bidimensional array from its string representation"
  ;(loop [m [], ms m-str, i 0]
  (loop [m [], ms m-str]
    (let [not-newline? (fn [c] (not (= \newline c)))
          drop-row (fn [c] (drop 1 (drop-while not-newline? c)))
          row (take-while not-newline? ms)]
      (if (empty? row)
        m
        ;(recur (assoc m i (make-row row)), (drop-row ms), (inc i))))))
        (recur (conj m (make-row row)) (drop-row ms))))))

(defn prod-vec-part [v group-size]
  "Computes the max product of each group of ints of 
   size group-size from the vector v"
  (if (< (count v) group-size)
    (reduce * v)
    (let [groups (partition group-size 1 v);]
          _ (prn "v=" v)
          _ (prn "groups=" groups)]
      (apply max (map (partial reduce *) groups)))))

(defn max-prod-rows [m]
  (do
  (prn "max-prod-rows, m=" m)
  (apply max (map #(prod-vec-part % 4) m))))

(defn vert [original-m]
  ""
  ;(loop [m original-m, vm [], i 0]
  (loop [m original-m, vm []]
    (if (empty? (first m))
      vm
      (let [row (map first m)]
        (recur (map (partial drop 1) m), (conj vm row)))))) 

(defn transf-matrix-to-vec [matrix]
  (loop [m matrix, v []]
    (if (empty? (first m))
      v
      (recur (drop 1 m) (apply conj v (first m))))))

(defn make-diag-a [vect index-start dir n]
  (let [op ({:up -, :down +} dir)
        out-of-bounds? (fn [i] (or (< i 0) (>= i (* n n))))]
        ;_ (prn )
        ;_ (prn )
        ;_ (prn "enter make-diag, index-start=" index-start)] 
    (loop [i index-start, diag []]
      (if (out-of-bounds? i)
        (do
          ;(prn "diag compl=" diag)
          ;diag)
          (take n  diag))
        (do
          ;(prn "diag=" diag)
          ;(prn "i=" i "e=" (vect i))
        (recur (op i (dec n)) (conj diag (vect i))))))))

(defn make-diag-b [vect index-start dir n]
  (let [op ({:up -, :down +} dir)
        out-of-bounds? (fn [i] (or (< i 0) (>= i (* n n))))]
        ;_ (prn )
        ;_ (prn )
        ;_ (prn "enter make-diag, index-start=" index-start)] 
    (loop [i index-start, diag []]
      (if (out-of-bounds? i)
        (do
          ;(prn "diag compl=" diag)
          ;diag)
          (take n  diag))
        (do
          ;(prn "diag=" diag)
          ;(prn "i=" i "e=" (vect i))
        (recur (op i (inc n)) (conj diag (vect i))))))))

(defn upwards-1st-col [vect n]
  (loop [i 0, a []]
    (if (= i (* n n))
      (do
        (prn "upwards-1st-col ret=" a)
        a)
      (do
        ;(prn "i=" i)
        (recur (+ i n), (conj a (make-diag-a vect i :up n)))))))

(defn downwards-last-col [vect n]
  (loop [i (dec n), a[]]
    (if (> i (* n n))
      (do
      (prn "downwards-last-col=" a)
      a)
      (recur (+ i n), (conj a (make-diag-a vect i :down n))))))

(defn downwards-1st-col [vect n]
  (loop [i 0, a []]
    (if (>= i (* n n))
      (do
      (prn "downwards-1st-col=" a)
      a)
      (recur (+ i n) (conj a (make-diag-b vect i :down n))))))

(defn upwards-last-col [vect n]
  (loop [i (dec n), a []]
    (if (>= i (* n n))
      (do
      (prn "upwards-last-col=" a)
      a)
      (recur (+ i n) (conj a (make-diag-b vect i :up n))))))


  
(defn diag-a [vect n]
  "Diagonal starting on the left and descending to the right"
;  (let [v []]
    ;(apply conj v (upwards-1st-col vect n) (right-last-row vect n)))) 
    (apply conj (upwards-1st-col vect n) (downwards-last-col vect n))) 

(defn diag-b [vect n]
  "Diagonal starting on the left and descending to the right"
    ;(apply conj v (upwards-1st-col vect n) (right-last-row vect n)))) 
    (apply conj (downwards-1st-col vect n) (upwards-last-col vect n))) 


(defn prod []
  (let [m (build-matrix matrix-as-str)
        n (count m)
        m-as-vect (transf-matrix-to-vec m)
        da (diag-a m-as-vect n)
        ;_ (prn "da=" da)
        ;_ (prn "max prod row=" (max-prod-rows da))
        db (diag-b m-as-vect n)
        _ (prn "db=" db)
        _ (prn "Done printing db")
        ;_ (prn "max prod row=" (max-prod-rows db))
        ;_ (prn "a=" a)
        ;_ (prn "vert-a=" (vert a))
        ;dirs [m (vert m) da db]
        dirs [m (vert m) da db]]
        ;_ (prn "dirs=" dirs)]
        ;_ (prn "vec=" m-as-vect)
    (apply max (map max-prod-rows dirs))))

