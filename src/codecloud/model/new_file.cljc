(ns codecloud.model.new-file
  "What a file made from the cloud starts as. Its name is checked as it is
  typed, and a Clojure source file starts with an ns form naming the
  namespace its place implies, read from where the cloud's namespaces
  keep their files."
  (:require [clojure.string :as string]
            [codecloud.model.source :as source]))

(def ^:private namespace-extensions
  "Extensions of the Clojure files that start with an ns form."
  #{"clj" "cljs" "cljc"})

(defn name-problem
  "Why `typed`, a new file's path relative to the cloud's folder, cannot
  be taken, or nil."
  [typed]
  (let [path (string/trim (str typed))]
    (cond
      (string/blank? path) "Name the file."
      (or (re-find #"^(?:[/\\]|[A-Za-z]:)" path)
          (some #{".."} (string/split path #"[/\\]"))) "Give a path inside the cloud's folder."
      (re-find #"[/\\]$" path) "End the path with the file's name."
      :else nil)))

(defn- without-extension [path]
  (string/replace path #"\.[^./]+$" ""))

(defn- namespace-path
  "Where namespace `namespace-name` keeps its file below its source root,
  without the extension, as app/my_core for app.my-core."
  [namespace-name]
  (-> namespace-name (string/replace "-" "_") (string/replace "." "/")))

(defn source-roots
  "The folders the Clojure namespaces of `snapshot` keep their files
  below, the deepest first, as src/ for app.core in src/app/core.clj."
  [snapshot]
  (->> (for [[_ {namespace-name :name :keys [files]}] (:namespaces snapshot)
             {:keys [path]} files
             :when (and namespace-name (contains? namespace-extensions (source/file-extension path)))
             :let [stem (without-extension path)
                   tail (namespace-path namespace-name)]
             :when (string/ends-with? stem tail)
             :let [root (subs stem 0 (- (count stem) (count tail)))]
             :when (or (= "" root) (string/ends-with? root "/"))]
         root)
       distinct
       (sort-by count >)))

(defn namespace-name
  "The namespace the Clojure file at `path` names by its place below the
  deepest source root holding it, or below a top-level src, test, or dev
  folder."
  [snapshot path]
  (let [stem (without-extension path)
        root (or (some #(when (string/starts-with? stem %) %) (source-roots snapshot))
                 (re-find #"^(?:src|test|dev)/" stem)
                 "")]
    (-> (subs stem (count root))
        (string/replace "/" ".")
        (string/replace "_" "-"))))

(defn starting-text
  "[text line column] of a new file at `path`, zero-based. A Clojure
  source file starts with its ns form, the caret below it."
  [snapshot path]
  (if (contains? namespace-extensions (source/file-extension path))
    [(str "(ns " (namespace-name snapshot path) ")\n\n\n") 2 0]
    ["" 0 0]))
