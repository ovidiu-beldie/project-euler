;In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

;The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

;What is the greatest product of four adjacent numbers in any direction (up, down, left,
;right, or diagonally) in the 20×20 grid?

(def the-matrix 
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

(require '[clojure.string :as str])

(declare format-matrix directional-delta)

(defn p11
  "Solution to P11"
  ([] (p11 4))
  ([n]
     (let [m (format-matrix)
           [max_id_rows max_id_cols] (key (last m))
           ev? (partial elems-valid? (inc max_id_rows) (inc max_id_cols))]
       (apply max (for [e m, d directional-delta
                        :let [elems (select-elems (key e) n d)]
                        :when (ev? elems)]
                    (apply * (map (partial get m) elems)))))))

(def directional-delta
  "Defines an incremental movement in any of the 4 directions:
0°, -90°, -45°, -135° (using a trigonometric circle reference)"
  [[0 1] [1 0] [1 1] [1 -1]])

(defn next-elem
  "Takes the deltas for a direction and the current element's coord's.
Returns the coord's of the next element in the designated direction"
  [[delta-row delta-col] [row col]]
  [(+ row delta-row) (+ col delta-col)])

(defn select-elems
  "Takes a starting element, the number of elements to select
and the direction where to take next elem's from. Returns the
coordinates of the selected elem's. These coordinates MAY be
out of bounds of the matrix on either side and thus must be validated."
  [start-elem n dir]
  (take n (iterate (partial next-elem dir) start-elem)))

(defn elems-valid?
  "Takes the nb of rows and cols and some elements' coord's.
Verifies if the elem's are within the matrix's bounds."
  [rows cols elems]
  (let [lower-than-max? (fn [[row-id col-id]]
                   (and (< row-id rows) (< col-id cols)))]
    (if (some neg? (flatten elems))
      false
      (every? lower-than-max? elems))))


;; Process matrix from a string to a map representation

(defn assoc-coords-to-val
  "Takes a map entry which associates a row's index
with its content (a seq). Produces a seq of elements
of the form '[[row col] val]'"
  [[row-ind row-vals]] 
  (let [indexed-entry (fn [i [j v]]
                        [[i j] v])]
    (->> (interleave (iterate inc 0) row-vals)
         (apply sorted-map)
         (seq)
         (map (partial indexed-entry row-ind)))))

(defn format-matrix
  "Takes the string version of the matrix and creates a seq
of maps, each representing an element (including its cartesian
coordinates"
  ([] (format-matrix the-matrix))
  ([mat]
   (let [split-row (fn [v] (str/split (first v) #"\s")) ;
         str-to-int (fn [v] (map #(Integer/valueOf %) v))]
     (->> mat
          str/split-lines                ;1 str per row
          (map str/trim)                 
          (map list)                     ;put each row in a list
          (map split-row)                ;split row str in 1 str per number
          (map str-to-int)               ;convert the numbers from str to int
          (interleave (iterate inc 0))   ;index the seq of rows
          (apply sorted-map)             ;create a map with the row index as key and row as val
          (map assoc-coords-to-val)      
          (reduce into)                  ;merge row into 1 seq
          (reduce into)                  ;merge all element vec into one
          (apply sorted-map)))))         ;create map with entry: '[row col] val'




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Previous solution (march 2012)

(defn make-row [r]
  "Creates a vector of ints from a list of seqs containing the int digits"
  (let [make-int-from-seq (fn [s] (Integer/valueOf (apply str s)))]
  (map make-int-from-seq (partition 2 3 (.trim (apply str r))))))

(defn build-matrix [m-str]
  "Build the bidimensional array from its string representation"
  (loop [m [], ms m-str]
    (let [not-newline? (fn [c] (not (= \newline c)))
          drop-row (fn [c] (drop 1 (drop-while not-newline? c)))
          row (take-while not-newline? ms)]
      (if (empty? row)
        m
        (recur (conj m (make-row row)) (drop-row ms))))))

(defn prod-vec-part [v group-size]
  "Computes the max product of each group of ints of 
   size group-size from the vector v"
  (if (< (count v) group-size)
    (reduce * v)
    (let [groups (partition group-size 1 v)]
      (apply max (map (partial reduce *) groups)))))

(defn max-prod-rows [g-size m]
  "Computes the maximum product of all the g-size groups of ints
    from the seq of vectors m"
  (apply max (map #(prod-vec-part % g-size) m)))

(defn transpose [original-m]
  "Transposes a matrix"
  (loop [m original-m, vm []]
    (if (empty? (first m))
      vm
      (let [row (map first m)]
        (recur (map (partial drop 1) m), (conj vm row)))))) 

(defn transf-matrix-to-vec [matrix]
  "Creates a unidimensional representation of the matrix"
  (loop [m matrix, v []]
    (if (empty? (first m))
      v
      (recur (drop 1 m) (apply conj v (first m))))))

(defn out-of-bounds? [i n]
  "Checks if index is out of the bounds of the sqaure" 
  (or (< i 0) (>= i (* n n))))


(defn make-diag [vect index-start diag-type dir n]
  "Creates a diagonal with values from a matrix of size n
   represented as the vector vect. 
   index-start is the unidimensional index of the start element of the diagonal. 
   diag-type is the type of diagonal (see below)
   dir is the direction to follow to create the dialog"
  (let [op ({:up -, :down +} dir)
        delta-op ({:a inc, :b dec} diag-type)]
    (loop [i index-start, diag []]
      (if (out-of-bounds? i n)
        (take n  diag)
        (recur (op i (delta-op n)) (conj diag (vect i)))))))

(defn make-diag-half-seq [vect n diag-type i-init dir]
  "Creates a sequence of diagonals of type diag-type for a matrix of size n
   reprsented as the vector vect. The series of diagonals are created by 
   looping through all the values of either the first or the last column
   add forming a diagonal by taking neighbours either upwards or downwards.
   i-init is the index of first value on either the first or last column
   and dir is the direction to take to form columns (:up or :down)"
  (loop [i i-init, a []]
    (if (out-of-bounds? i n)
      a
      (recur (+ i n) (conj a (make-diag vect i diag-type dir n))))))

(defn make-diag-seq [vect n diag-type]
  "Make a sequence of diagonals from a matrix of size n.
   vect is the matrix represented as a vector.
   The diagonals can be of type :a - NW->SE of :b - NE -> SW"
  (let [diag-maker (partial make-diag-half-seq vect n diag-type)
        index-start-down ({:a 0, :b (dec n)} diag-type)  
        index-start-up ({:a (dec n), :b 0} diag-type)]
    (apply conj (diag-maker index-start-down :down) (diag-maker index-start-up :up))))

(defn prod []
  (let [m (build-matrix the-matrix)
        n (count m)
        m-as-vect (transf-matrix-to-vec m)
        da (make-diag-seq m-as-vect n :a)
        db (make-diag-seq m-as-vect n :b)
        max-prod (partial max-prod-rows 4)
        dirs [m (transpose m) da db]]
    (apply max (map max-prod dirs))))

