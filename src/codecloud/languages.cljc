(ns codecloud.languages
  "Language adapters, turning a file's text into codecloud.model.facts.
  Languages without one read as plain text."
  (:require [clojure.string :as string]
            [codecloud.languages.c-family :as c-family]
            [codecloud.languages.catalog :as catalog]
            [codecloud.languages.css :as css]
            [codecloud.languages.html :as html]
            [codecloud.languages.javascript :as javascript]
            [codecloud.languages.manifests :as manifests]
            [codecloud.languages.plain :as plain]
            [codecloud.languages.python :as python]
            [codecloud.languages.shell :as shell]
            [codecloud.model.parse :as parse]
            [codecloud.model.source :as source]))

(defn- clojure-imports
  "Imports of a Clojure file that reach other languages: JavaScript
  requires, Java classes, and the scripts its shell calls run."
  [{:keys [requirements class-imports commands]}]
  (into []
        (concat
         (for [{:keys [target js-module?]} requirements
               :when js-module?]
           {:specifier target :kind :interop :resolver :javascript :binds [] :replaces-requirement target
            :drop-unresolved? true})
         (for [class-name class-imports]
           {:specifier class-name :kind :interop :resolver :java-class :binds [] :drop-unresolved? true})
         (for [{:keys [text line]} commands
               found (shell/command-imports text line)]
           (assoc found :drop-unresolved? true)))))

(defn- read-clojure
  "Facts of a Clojure file, whose unit is its namespace or else its path."
  [path text options]
  (let [facts (parse/analyze-file path text options)]
    (assoc facts
           :language :clojure
           :unit-id (or (:namespace facts) path)
           :imports (clojure-imports facts))))

(def adapters
  "Each language's reader, a function of the path, the text, and the
  options: the cloud file's :flags and :complection, and :parse-tree for
  the languages read through a syntax tree."
  {:clojure {:read read-clojure}
   :python {:read python/read-python}
   :javascript {:read (partial javascript/read-script :javascript)}
   :typescript {:read (partial javascript/read-script :typescript)}
   :css {:read css/read-style-sheet}
   :html {:read html/read-page}
   :shell {:read shell/read-script}
   :c {:read (partial c-family/read-file :c)}
   :cpp {:read (partial c-family/read-file :cpp)}})

(defn assign-units
  "File facts with the units only the whole set of files decides, such as
  a C header joining its source file's page."
  [file-facts]
  (c-family/paired-units file-facts))

(defn- first-line [text]
  (if-let [newline-index (string/index-of text "\n")]
    (subs text 0 newline-index)
    text))

(defn- newline-count [text]
  (loop [from 0
         found 0]
    (if-let [index (string/index-of text "\n" from)]
      (recur (inc index) (inc found))
      found)))

(defn skip-reason
  "Why a readable file is left out, or nil: :binary, :too-large, or
  :minified, by name or by lines averaging hundreds of characters."
  [path text]
  (let [binary-sample 1024
        largest-text 1000000
        minified-average 300
        smallest-minified 2000]
    (cond
      (string/includes? (subs text 0 (min (count text) binary-sample)) "\u0000") :binary
      (> (count text) largest-text) :too-large
      (re-find #"\.min\.[^/.]+$" path) :minified
      (and (> (count text) smallest-minified)
           (> (/ (count text) (inc (newline-count text))) minified-average)) :minified
      :else nil)))

(defn- guarded-read
  "Facts from `read-file`, or plain facts with a :parse-error when the
  adapter fails, so one file never stops a whole cloud."
  [read-file language-id path text options]
  (try
    (read-file path text options)
    (catch #?(:clj Exception :cljs :default) error
      (assoc (plain/read-plain language-id path text options)
             :parse-error {:message (str "Could not analyze this file: " (or (ex-message error) error))}))))

(defn analyze-file
  "Facts of the file at `path`, or nil when left out. Manifests read as
  {:path :manifest}. `options` carry :flags, :complection, and
  :parse-tree, a function of a grammar id, a text, and a query that the
  host passes once tree-sitter has loaded."
  [path text options]
  (let [file-name (source/file-name path)
        language (catalog/language-for path (first-line text))]
    (cond
      (contains? manifests/file-names file-name) (manifests/read-manifest path file-name text)
      (or (skip-reason path text) (and (nil? language) (catalog/extensionless? path))) nil
      :else (let [language-id (:id language :plain)
                  read-file (get-in adapters [language-id :read] (partial plain/read-plain language-id))]
              (guarded-read read-file language-id path text options)))))
