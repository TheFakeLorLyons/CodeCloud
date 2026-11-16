(ns codecloud.counting-test
  (:require [clojure.test :refer [deftest is]]
            [codecloud.counting :as counting]))

(deftest counts-read-the-same-everywhere
  (is (= ["0" "7" "999" "1,000" "12,840" "123,456" "1,234,567" "-4,096" "0"]
         (map counting/grouped-digits [0 7 999 1000 12840 123456 1234567 -4096 nil])))
  (is (= "1 page" (counting/counted 1 "page")))
  (is (= "3,204 pages" (counting/counted 3204 "page")))
  (is (= "2 dependencies" (counting/counted 2 "dependency" "dependencies")))
  (is (= "0 lines" (counting/counted nil "line")) "no count counts as none"))

(deftest times-read-as-how-long-ago
  (let [now 10000000
        minute 60000
        hour (* 60 minute)
        day (* 24 hour)]
    (is (= ["a moment ago" "2 minutes ago" "89 minutes ago" "1 hour ago" "3 hours ago" "a moment ago"]
           (map #(counting/ago now (- now %)) [(* 1000 89) (* 2 minute) (* 89 minute) (* 90 minute) (* 200 minute) (- minute)])))
    (is (= ["35 hours ago" "1 day ago" "44 days ago" "1 month ago" "13 months ago" "1 year ago"]
           (map #(counting/ago now (- now %)) [(* 35 hour) (* 36 hour) (* 44 day) (* 45 day) (* 400 day) (* 600 day)])))))
