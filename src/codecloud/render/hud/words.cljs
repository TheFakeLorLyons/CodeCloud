(ns codecloud.render.hud.words
  "How the overlay words scores, snapshots, and names."
  (:require [clojure.string :as string]
            [codecloud.counting :as counting]))

(defn points
  "A complection score in words, to one decimal place."
  [score]
  (let [rounded (/ (js/Math.round (* 10 (or score 0))) 10)]
    (str (.toLocaleString rounded "en-US") (if (= 1 rounded) " point" " points"))))

(defn snapshot-caption
  "What the shown snapshot is, the working tree or a commit's subject and
  age."
  [{:keys [snapshot commits]}]
  (let [snapshot-id (:id snapshot)]
    (if (or (nil? snapshot-id) (= :working-tree snapshot-id))
      "Working tree"
      (let [commit (some #(when (= snapshot-id (:sha %)) %) commits)]
        (str (:subject commit) ", " (counting/ago (js/Date.now) (:time commit)))))))

(defn capitalized
  "`text` with its first letter in capitals and the rest as written."
  [text]
  (if (string/blank? text)
    text
    (str (string/upper-case (subs text 0 1)) (subs text 1))))

(defn first-paragraph [doc]
  (when doc
    (let [paragraph (string/trim (first (string/split (string/trim doc) #"\n\s*\n")))]
      (string/replace paragraph #"\s+" " "))))

(defn name-heading [{:keys [prefix leaf]}]
  (list [:span.name-prefix prefix] [:span.name-leaf leaf]))
