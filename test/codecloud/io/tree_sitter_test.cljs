(ns codecloud.io.tree-sitter-test
  "Checks that the recorded tree-sitter matches the JVM tests read are the
  grammars' own. With CODECLOUD_RECORD set, records them again."
  (:require ["fs" :as fs]
            ["path" :as path]
            [cljs.test :refer [async deftest is]]
            [clojure.string :as string]
            [codecloud.fixtures :as fixtures]
            [codecloud.io.tree-sitter :as tree-sitter]
            [codecloud.languages.c-family :as c-family]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.source :as source]))

(defn- recorded-paths
  "The C and C++ files whose matches are recorded, relative to the
  project's root."
  []
  (conj (vec (for [[path text] (fixtures/file-texts fixtures/polyglot-root)
                   :when (:syntax-tree? (catalog/language-for path (first (string/split-lines text))))]
               (str fixtures/polyglot-root "/" path)))
        "test-resources/c-family/sample.cpp"))

(defn- live-matches
  "{path {grammar matches}} for each recorded file, parsed now with the
  grammar the C family adapter would choose."
  [parse]
  (into (sorted-map)
        (for [file-path (recorded-paths)
              :let [text (source/normalize-text (fixtures/read-text file-path))
                    language (c-family/file-language (:id (catalog/language-by-name file-path)) file-path
                                                     (source/split-lines text))]]
          [file-path {language (parse language text (get c-family/queries language))}])))

(defn- recording-text
  "The recording as EDN, one match to a line, so changes read as diffs."
  [recorded]
  (str ";; What tree-sitter's grammars answer to the C family adapter's queries"
       " for the C and C++ test files.\n"
       ";; Recorded by running the tests with CODECLOUD_RECORD set.\n"
       "{"
       (string/join "\n "
                    (for [[file-path by-grammar] recorded
                          [grammar matches] by-grammar]
                      (str (pr-str file-path) "\n {" (pr-str grammar) "\n  ["
                           (string/join "\n   " (map pr-str matches))
                           "]}")))
       "}\n"))

(deftest recorded-matches-are-the-grammars'-own
  (async done
    (-> (tree-sitter/load-parser (path/resolve "dist"))
        (.then (fn [{:keys [parse error]}]
                 (is (nil? error))
                 (when parse
                   (let [live (live-matches parse)]
                     (if (.. js/process -env -CODECLOUD_RECORD)
                       (.writeFileSync fs (str "test-resources/" fixtures/tree-sitter-recording) (recording-text live))
                       (is (= (fixtures/expected fixtures/tree-sitter-recording) live)
                           "The grammars or queries changed: run the tests with CODECLOUD_RECORD=1 to record them again."))))
                 (done))))))
