(ns codecloud.counting
  "Counts and times in words, written the same on every platform.")

(defn grouped-digits
  "A whole number with its thousands set apart by commas, as 12,840, and
  nil as 0."
  [number]
  (let [number (or number 0)
        group-size 3]
    (if (neg? number)
      (str "-" (grouped-digits (- number)))
      (let [digits (str number)
            lead-size (let [leftover (rem (count digits) group-size)] (if (zero? leftover) group-size leftover))]
        (apply str (subs digits 0 lead-size)
               (map #(apply str "," %) (partition group-size (subs digits lead-size))))))))

(defn counted
  "`number` with the noun it counts, as 1 page or 3,204 pages, and nil
  counted as 0."
  ([number noun] (counted number noun (str noun "s")))
  ([number noun plural]
   (str (grouped-digits number) " " (if (= 1 number) noun plural))))

(defn ago
  "How long before `now` the moment `at` was, both in milliseconds, in
  words. Each unit takes over once one and a half of it have passed, so
  89 minutes read as minutes and 90 as an hour."
  [now at]
  (let [seconds (quot (max 0 (- now at)) 1000)
        units [["minute" 60] ["hour" 3600] ["day" 86400] ["month" 2592000] ["year" 31536000]]
        [unit unit-seconds] (last (filter (fn [[_ unit-seconds]] (>= seconds (* 1.5 unit-seconds))) units))]
    (if unit
      (str (counted (quot seconds unit-seconds) unit) " ago")
      "a moment ago")))
