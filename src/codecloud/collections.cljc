(ns codecloud.collections
  "Small collection helpers that clojure.core leaves out."
  (:require [clojure.string :as string]))

(defn distinct-by
  "Vector of the items whose `key-fn` value has not appeared before,
  keeping the first item for each key in its original order."
  [key-fn items]
  (let [seen-keys (volatile! (transient #{}))]
    (persistent!
     (reduce (fn [kept-items item]
               (let [item-key (key-fn item)]
                 (if (contains? @seen-keys item-key)
                   kept-items
                   (do (vswap! seen-keys conj! item-key)
                       (conj! kept-items item)))))
             (transient [])
             items))))

(defn clamp
  "Limits a number to the closed range from `low` to `high`."
  [low high value]
  (max low (min high value)))

(defn spoken-list
  "Joins items the way a sentence lists them, as in a, a and b, or a, b,
  and c, with `conjunction` in place of and when given."
  ([items] (spoken-list items "and"))
  ([items conjunction]
   (let [items (vec items)]
     (case (count items)
       0 ""
       1 (str (first items))
       2 (str (first items) " " conjunction " " (second items))
       (str (string/join ", " (pop items)) ", " conjunction " " (peek items))))))
