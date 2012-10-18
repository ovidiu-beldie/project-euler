;You are given the following information, but you may prefer to do some research for yourself.

;    1 Jan 1900 was a Monday.
;    Thirty days has September,
;    April, June and November.
;    All the rest have thirty-one,
;    Saving February alone,
;    Which has twenty-eight, rain or shine.
;    And on leap years, twenty-nine.
;    A leap year occurs on any year evenly divisible by 4, but not on a century unless
;it is divisible by 400.

;How many Sundays fell on the first of the month during the twentieth century
;(1 Jan 1901 to 31 Dec 2000)?

(def days-of-week [:mon :tue :wed :thu :fri :sat :sun])

(defn classify-month
  "Classifies months in 4 categories: 30-day months, 'regular' 31-day months,
february and december"
  [[_ m _]]
  (cond
   (#{:feb :dec} m) m
   (#{:apr :jun :sep :nov} m) :30-day-month
   :else :reg-31-day-month))

(defn next-month [m]
  (let [months [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]]
    (second (drop-while #(not (= m %)) (cycle months)))))

(defmulti next-day
  "Takes a calendar date. Returns the next calendar day"
  classify-month)

(defmethod next-day :feb [[d m y]]
  (let [leap-year? (fn []
                     (cond
                      (zero? (mod y 100)) (zero? (mod y 400))
                      (zero? (mod y 4)) true
                      :else false))]
   (cond
    (< d 28) [(inc d) m y]
    (= d 29) [1 :mar y]
    :else (if (leap-year?) [29 :feb y]
              [1 :mar y]))))

(defmethod next-day :dec [[d m y]]
  (if (< d 32) [(inc d) m y]
      [1 :jan (inc y)]))

(defmethod next-day :30-day-month [[d m y]]
  (if (< d 30) [(inc d) m y]
      [1 (next-month m) y]))

(defmethod next-day :reg-31-day-month [[d m y]]
  (if (< d 31) [(inc d) m y]
      [1 (next-month m) y]))

(defn p19
  "Solution to P19"
  ([] (p19 [31 :dec 2000]))
  ([end-date]
     (->> (iterate next-day [1 :jan 1901])
          (take-while #(not (= (next-day end-date) %)))
          (interleave (cycle days-of-week))
          (partition 2)
          (filter #(and (= :sun (first %)) (= 1 (first (second %)))))
          (count))))

