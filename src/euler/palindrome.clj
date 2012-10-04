(ns euler.palindrome)

(defn palindrome?
  "Returns true if the parameter is a palindrome"
  [n]
  (let [n-str (str n)]
    (= n-str (apply str (reverse n-str)))))