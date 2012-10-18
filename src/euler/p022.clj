;Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over
;five-thousand first names, begin by sorting it into alphabetical order. Then working out the
;alphabetical value for each name, multiply this value by its alphabetical position in the list
;to obtain a name score.

;For example, when the list is sorted into alphabetical order, COLIN, which is worth
;3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score
;of 938 × 53 = 49714.

;What is the total of all the name scores in the file?

(require '[clojure.string :as str])

(defn format-file-contents
  "Returns a seq of the strings contained in the file"
  [file-name]
  (str/split
   (->> (slurp file-name)
        (replace {\, \space })
        (remove #{\"})
        (apply str))
   #"\s"))

(defn alphabetical-value
  "Returns the alphabetical value of a word as defined by the problem"
  [s]
  (let [alphabet (->> (range 65 91)
                      (map (comp #(Character. %) char)))
        indexed-alphabet (apply hash-map (interleave alphabet (iterate inc 1)))]
    (reduce #(+ %1 (indexed-alphabet %2)) 0 s)))

(defn p22
  "Solution to P22"
  [file]
  (->> file
       (format-file-contents)
       (sort)
       (map alphabetical-value)
       (interleave (iterate inc 1))
       (partition 2)
       (reduce #(+ %1 (* (first %2) (second %2))) 0)))

