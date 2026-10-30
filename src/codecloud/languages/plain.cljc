(ns codecloud.languages.plain
  "Facts every text file has: its lines, longest line, and text flags.
  Other adapters build on these."
  (:require [codecloud.model.complection :as complection]
            [codecloud.model.source :as source]))

(defn base-facts
  "Facts of the file at `path` in `language-id`, with no requirements or
  definitions yet. The file is its own unit."
  [language-id path normalized-text text-lines {:keys [flags]}]
  {:path path
   :language language-id
   :unit-id path
   :dialect (source/file-extension path)
   :line-count (count text-lines)
   :longest-line (reduce max 0 (map count text-lines))
   :requirements []
   :imports []
   :definitions []
   :top-level-forms []
   :measured-forms []
   :flag-counts (complection/text-flags (complection/compile-flags flags) normalized-text)
   :parse-error nil})

(defn read-plain
  "Facts of a file read as plain text in `language-id`."
  [language-id path text options]
  (let [normalized-text (source/normalize-text text)]
    (base-facts language-id path normalized-text (source/split-lines normalized-text) options)))
