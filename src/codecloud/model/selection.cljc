(ns codecloud.model.selection
  "Path selections in gitignore syntax, an include list, which takes
  everything when empty, narrowed by an ignore list."
  (:require [codecloud.model.ignore :as ignore]))

(defn compile-selection
  "Matcher for a selection map of :include and :ignore pattern lists."
  [{include-patterns :include ignore-patterns :ignore}]
  {:include-rules (when (seq include-patterns) [(ignore/rule-set "" include-patterns)])
   :ignore-rules (when (seq ignore-patterns) [(ignore/rule-set "" ignore-patterns)])})

(defn selected? [{:keys [include-rules ignore-rules]} path]
  (and (or (nil? include-rules) (ignore/ignored? include-rules path))
       (not (and ignore-rules (ignore/ignored? ignore-rules path)))))

(defn directory-pattern
  "Pattern matching everything inside the folder at `path`, anchored to
  the root. The root folder, \"\", matches everything."
  [path]
  (if (= "" path) "/**" (str "/" path "/")))

(defn file-pattern [path]
  (str "/" path))
