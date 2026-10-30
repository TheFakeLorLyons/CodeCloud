(ns codecloud.io.git
  "The git command line: working tree files, history, and contents at any
  commit, with paths relative to the cloud's root."
  (:require [clojure.string :as string]
            [codecloud.io.process :as process]
            [codecloud.io.task :as task]))

(defn- run-git
  "Runs git and resolves to its standard output, rejecting with git's own
  message when it exits with an error."
  [git-executable directory arguments & {:as options}]
  (task/then (process/run git-executable arguments (assoc options :directory directory))
             (fn [{:keys [exit-code stdout stderr]}]
               (if (zero? exit-code)
                 stdout
                 (task/rejected (js/Error. (str "git " (first arguments) " failed: "
                                                (string/trim stderr))))))))

(defn repository-root
  "Resolves to the repository's top-level directory, or nil outside a
  repository or without git."
  [git-executable directory]
  (-> (run-git git-executable directory ["rev-parse" "--show-toplevel"])
      (task/then string/trim)
      (task/recover (fn [_error] nil))))

(defn- split-null-separated [output]
  (into [] (remove string/blank?) (string/split output #"\u0000")))

(defn working-tree-files
  "Tracked files plus untracked files that .gitignore, .git/info/exclude,
  and the global excludes file do not ignore."
  [git-executable directory]
  (task/then (run-git git-executable directory
                      ["-c" "core.quotePath=false" "ls-files" "-z" "--cached" "--others" "--exclude-standard"])
             (fn [output] (vec (distinct (split-null-separated output))))))

(def ^:private field-separator "\u001f")
(def ^:private record-separator "\u001e")

(defn- parse-commit-record [record]
  (let [[sha seconds author subject] (string/split (string/trim record) (re-pattern field-separator))
        object-name-lengths #{40 64}] ; hex digits in SHA-1 and SHA-256 object names
    (when (and sha (contains? object-name-lengths (count sha)))
      {:sha sha
       :time (* 1000 (js/parseInt seconds 10))
       :author author
       :subject subject})))

(defn commits
  "Up to `limit` commits touching files with `extensions`, newest first,
  as {:sha :time :author :subject}."
  [git-executable directory {:keys [limit extensions]}]
  (task/then (run-git git-executable directory
                      (into ["log" (str "--max-count=" limit)
                             (str "--format=%H" field-separator "%ct" field-separator
                                  "%an" field-separator "%s" record-separator)
                             "--"]
                            (map #(str "*." %))
                            extensions))
             (fn [output]
               (into [] (keep parse-commit-record) (string/split output (re-pattern record-separator))))))

(defn- parse-tree-entry [entry]
  (let [tab-index (string/index-of entry "\t")]
    (when tab-index
      (let [[_mode object-type object-id] (string/split (subs entry 0 tab-index) #" ")]
        (when (= "blob" object-type)
          {:path (subs entry (inc tab-index))
           :blob object-id})))))

(defn tree-files
  "Every file in commit `sha` under the root, as {:path :blob}."
  [git-executable directory sha]
  (task/then (run-git git-executable directory ["-c" "core.quotePath=false" "ls-tree" "-r" "-z" sha])
             (fn [output] (into [] (keep parse-tree-entry) (split-null-separated output)))))

(defn- parse-batch-output
  "Splits `git cat-file --batch` output into {object-id text}. Sizes count
  bytes, so it parses the raw buffer."
  [^js buffer]
  (let [newline-byte 10]
    (loop [position 0
           texts (transient {})]
      (let [header-end (.indexOf buffer newline-byte position)]
        (if (neg? header-end)
          (persistent! texts)
          (let [[object-id object-type size-text] (string/split (.toString buffer "utf8" position header-end) #" ")
                content-start (inc header-end)]
            (if (or (= "missing" object-type) (nil? size-text))
              (recur content-start texts)
              (let [content-end (+ content-start (js/parseInt size-text 10))]
                (recur (inc content-end)
                       (assoc! texts object-id (.toString buffer "utf8" content-start content-end)))))))))))

(defn read-blobs
  "Resolves to {blob-id text} for the requested blob ids, read in one git
  process."
  [git-executable directory blob-ids]
  (if (empty? blob-ids)
    (task/resolved {})
    (task/then (run-git git-executable directory ["cat-file" "--batch"]
                        :input (str (string/join "\n" blob-ids) "\n")
                        :binary? true)
               parse-batch-output)))
