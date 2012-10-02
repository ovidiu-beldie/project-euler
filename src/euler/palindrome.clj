(ns euler.palindrome)

(defn palindrome?
  [n]
  (let [n-str (str n)]
    (= n-str (apply str (reverse n-str)))))