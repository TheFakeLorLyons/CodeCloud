(ns codecloud.model.source
  "Text and path helpers shared by the parsers, the host, and the view.
  Paths use forward slashes."
  (:require [clojure.string :as string]))

(defn normalize-text
  "`text` without a leading byte order mark and with every line ending as
  LF, so line and column numbers agree with the editor's."
  [text]
  (let [without-mark (if (string/starts-with? text "\uFEFF") (subs text 1) text)]
    (-> without-mark
        (string/replace "\r\n" "\n")
        (string/replace "\r" "\n"))))

(defn split-lines
  "Lines of normalized text, counted as editors count them, where a final
  newline ends the last line and starts no empty one."
  [normalized-text]
  (if (= "" normalized-text)
    []
    (let [lines (string/split normalized-text #"\n" -1)]
      (if (= "" (peek lines)) (pop lines) lines))))

(defn line-count
  "Number of lines an editor shows for normalized text."
  [normalized-text]
  (count (split-lines normalized-text)))

(defn directory-of
  "The folder part of a path, or \"\" for a path in no folder."
  [path]
  (let [slash-index (string/last-index-of path "/")]
    (if (and slash-index (pos? slash-index)) (subs path 0 slash-index) "")))

(defn file-name
  "The last segment of a path."
  [path]
  (subs path (inc (or (string/last-index-of path "/") -1))))

(defn file-extension
  "Lower-case extension of a path, without its dot, or nil."
  [path]
  (let [name-part (file-name path)
        dot-index (string/last-index-of name-part ".")]
    (when (and dot-index (pos? dot-index))
      (string/lower-case (subs name-part (inc dot-index))))))
