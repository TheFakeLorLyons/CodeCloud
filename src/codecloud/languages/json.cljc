(ns codecloud.languages.json
  "Reads manifest JSON, with comments and trailing commas, into data with
  string keys."
  (:require [clojure.string :as string]))

(defn- failure [message index]
  (ex-info message {:index index}))

(defn- character-at [text index]
  (when (< index (count text)) (subs text index (inc index))))

(defn- blank-end
  "Index of the first character at or after `index` that is not blank or
  inside a comment."
  [text index]
  (loop [position index]
    (let [character (character-at text position)]
      (cond
        (nil? character) position
        (string/blank? character) (recur (inc position))
        (and (= "/" character) (= "/" (character-at text (inc position))))
        (recur (or (string/index-of text "\n" position) (count text)))
        (and (= "/" character) (= "*" (character-at text (inc position))))
        (recur (if-let [close (string/index-of text "*/" (+ 2 position))] (+ 2 close) (count text)))
        :else position))))

(def ^:private escaped-characters
  {"\"" "\"" "\\" "\\" "/" "/" "b" "\b" "f" "\f" "n" "\n" "r" "\r" "t" "\t"})

(defn- read-string-at
  "[text end] for the string whose opening quote is at `index`."
  [text index]
  (loop [position (inc index)
         pieces []]
    (let [character (character-at text position)]
      (cond
        (nil? character) (throw (failure "An unterminated string" index))
        (= "\"" character) [(apply str pieces) (inc position)]
        (= "\\" character)
        (let [escaped (character-at text (inc position))]
          (if (= "u" escaped)
            (let [digits (subs text (+ 2 position) (min (count text) (+ 6 position)))
                  code #?(:clj (Integer/parseInt digits 16) :cljs (js/parseInt digits 16))]
              (recur (+ 6 position) (conj pieces #?(:clj (str (char code)) :cljs (js/String.fromCharCode code)))))
            (recur (+ 2 position) (conj pieces (get escaped-characters escaped escaped)))))
        :else (recur (inc position) (conj pieces character))))))

(defn- text-at
  "Up to `length` characters of `text` from `index`."
  [text index length]
  (subs text index (min (count text) (+ index length))))

(defn- read-number-at [text index]
  (if-let [found (re-find #"^-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?" (text-at text index 64))]
    [#?(:clj (if (re-find #"[.eE]" found) (Double/parseDouble found) (Long/parseLong found))
        :cljs (js/parseFloat found))
     (+ index (count found))]
    (throw (failure "A value that is not JSON" index))))

(declare read-value-at)

(defn- read-items-at
  "[items end] for the array or object whose opening bracket is at
  `index`, closed by `closer`, reading each item with `read-item`."
  [text index closer read-item]
  (loop [position (blank-end text (inc index))
         items []]
    (cond
      (= closer (character-at text position)) [items (inc position)]
      (nil? (character-at text position)) (throw (failure "An unclosed bracket" index))
      :else
      (let [[item after-item] (read-item text position)
            after-blank (blank-end text after-item)]
        (if (= "," (character-at text after-blank))
          (recur (blank-end text (inc after-blank)) (conj items item))
          (recur after-blank (conj items item)))))))

(defn- read-entry-at
  "[[key value] end] for one entry of an object."
  [text index]
  (let [[entry-key after-key] (read-string-at text index)
        colon (blank-end text after-key)]
    (when-not (= ":" (character-at text colon))
      (throw (failure "An object entry without a colon" colon)))
    (let [[value after-value] (read-value-at text (blank-end text (inc colon)))]
      [[entry-key value] after-value])))

(defn- read-value-at
  "[value end] for the JSON value starting at `index`."
  [text index]
  (let [character (character-at text index)]
    (cond
      (= "{" character) (let [[entries end] (read-items-at text index "}" read-entry-at)]
                          [(into {} entries) end])
      (= "[" character) (read-items-at text index "]" read-value-at)
      (= "\"" character) (read-string-at text index)
      (= "true" (text-at text index 4)) [true (+ index 4)]
      (= "false" (text-at text index 5)) [false (+ index 5)]
      (= "null" (text-at text index 4)) [nil (+ index 4)]
      :else (read-number-at text index))))

(defn read-json
  "The value of JSON `text`, or nil when the text does not hold JSON."
  [text]
  (try
    (first (read-value-at text (blank-end text 0)))
    (catch #?(:clj Exception :cljs :default) _error
      nil)))
