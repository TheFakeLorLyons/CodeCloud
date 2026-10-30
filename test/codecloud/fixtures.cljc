(ns codecloud.fixtures
  "Reads the small repositories under test-resources, the same way on the
  JVM and on Node."
  (:require [clojure.string :as string]
            [codecloud.model.config :as config]
            #?(:clj [clojure.java.io :as io])))

(def polyglot-root
  "A repository in several languages, with files every adapter reads and
  files CodeCloud should leave out."
  "test-resources/polyglot")

(defn- relative-path [root full-path]
  (string/replace (subs full-path (inc (count root))) "\\" "/"))

(defn file-texts
  "Map of path, relative to `root` and written with forward slashes, to
  the text of every file under `root`."
  [root]
  #?(:clj (into (sorted-map)
                (comp (filter #(.isFile ^java.io.File %))
                      (map (fn [^java.io.File file]
                             [(relative-path root (string/replace (.getPath file) "\\" "/")) (slurp file)])))
                (file-seq (io/file root)))
     :cljs (let [fs (js/require "fs")
                 node-path (js/require "path")]
             (letfn [(walk [directory]
                       (mapcat (fn [entry]
                                 (let [full-path (.join node-path directory entry)]
                                   (if (.isDirectory (.statSync fs full-path))
                                     (walk full-path)
                                     [full-path])))
                               (sort (array-seq (.readdirSync fs directory)))))]
               (into (sorted-map)
                     (map (fn [full-path]
                            [(relative-path root (string/replace full-path "\\" "/"))
                             (.readFileSync fs full-path "utf8")]))
                     (walk root))))))

(defn read-text
  "The text of the file at `path`, relative to the project's root."
  [path]
  #?(:clj (slurp path)
     :cljs (.readFileSync (js/require "fs") path "utf8")))

(defn expected
  "The facts a fixture states about itself, read from its EDN file under
  test-resources."
  [file-name]
  (:value (config/read-edn (read-text (str "test-resources/" file-name)))))

(def tree-sitter-recording
  "The file under test-resources that records the matches tree-sitter
  gives for the C and C++ test files, which the JVM tests read in its
  place and the Node tests check against the grammars."
  "tree-sitter-matches.edn")

(def ^:private recorded-matches
  (delay (expected tree-sitter-recording)))

(defn recorded-parse-tree
  "A stand-in for tree-sitter, answering with the matches recorded for the
  file at `path`, relative to the project's root."
  [path]
  (fn [grammar _text _query]
    (get-in @recorded-matches [path grammar] [])))
