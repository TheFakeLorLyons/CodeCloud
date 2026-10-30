(ns codecloud.render.hud.words
  "How the overlay words counts, times, scores, and names."
  (:require [clojure.string :as string]))

(defn formatted-count [value]
  (.toLocaleString (or value 0) "en-US"))

(defn counted
  "A count with its noun, such as \"1 line\" or \"1,204 lines\"."
  [value singular plural]
  (str (formatted-count value) " " (if (= 1 value) singular plural)))

(defn points
  "A complection score in words, to one decimal place."
  [score]
  (let [rounded (/ (js/Math.round (* 10 (or score 0))) 10)]
    (str (.toLocaleString rounded "en-US") (if (= 1 rounded) " point" " points"))))

(defonce ^:private relative-time-format
  (js/Intl.RelativeTimeFormat. "en" #js {:numeric "auto"}))

(defn relative-time
  "Phrase such as \"3 days ago\" for an epoch time in milliseconds."
  [epoch-milliseconds]
  (let [^js formatter relative-time-format
        seconds (/ (- epoch-milliseconds (js/Date.now)) 1000)
        units [["year" 31536000] ["month" 2592000] ["week" 604800] ["day" 86400]
               ["hour" 3600] ["minute" 60] ["second" 1]]
        [unit size] (or (first (filter (fn [[_ size]] (>= (js/Math.abs seconds) size)) units))
                        ["second" 1])]
    (.format formatter (js/Math.round (/ seconds size)) unit)))

(defn snapshot-caption
  "What the shown snapshot is, the working tree or a commit's subject and
  age."
  [{:keys [snapshot commits]}]
  (let [snapshot-id (:id snapshot)]
    (if (or (nil? snapshot-id) (= :working-tree snapshot-id))
      "Working tree"
      (let [commit (some #(when (= snapshot-id (:sha %)) %) commits)]
        (str (:subject commit) ", " (relative-time (:time commit)))))))

(defn capitalized
  "`text` with its first letter in capitals and the rest as written."
  [text]
  (if (string/blank? text)
    text
    (str (string/upper-case (subs text 0 1)) (subs text 1))))

(defn first-paragraph
  "The first paragraph of a docstring on one line."
  [doc]
  (when doc
    (let [paragraph (string/trim (first (string/split (string/trim doc) #"\n\s*\n")))]
      (string/replace paragraph #"\s+" " "))))

(defn name-heading
  "Markup for a {:prefix :leaf} name, with the leaf set heavier."
  [{:keys [prefix leaf]}]
  (list [:span.name-prefix prefix] [:span.name-leaf leaf]))
