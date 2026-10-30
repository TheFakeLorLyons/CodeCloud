(ns codecloud.io.files
  "File access for the analyzer: reading text, hashing it as git does, and
  walking folders outside git."
  (:require ["crypto" :as crypto]
            ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.task :as task]
            [codecloud.model.ignore :as ignore]))

(defn absolute-path
  "Joins a root directory and a relative path that uses forward slashes,
  producing a path in the platform's own form."
  [root-directory relative-path]
  (apply path/join root-directory (string/split relative-path #"/")))

(defn read-text
  "Resolves to the UTF-8 contents of a file, or nil when it cannot be read
  (deleted, a directory, or not permitted)."
  [file-path]
  (-> (.readFile (.-promises fs) file-path "utf8")
      (task/recover (fn [_error] nil))))

(defn write-text
  "Writes UTF-8 text to a file, creating parent directories."
  [file-path text]
  (-> (.mkdir (.-promises fs) (path/dirname file-path) #js {:recursive true})
      (task/then (fn [_] (.writeFile (.-promises fs) file-path text "utf8")))))

(defn content-key
  "The git blob id of `text`, so a file on disk shares cache entries with
  the same blob from history."
  [text]
  (let [^js content (js/Buffer.from text "utf8")]
    (-> (.createHash crypto "sha1")
        (.update (str "blob " (.-length content) "\u0000"))
        (.update content)
        (.digest "hex"))))

(defn- directory-entries [directory]
  (-> (.readdir (.-promises fs) directory #js {:withFileTypes true})
      (task/then (fn [entries] (vec entries)))
      (task/recover (fn [_error] []))))

(defn- kept-entries
  "{:subdirectories :files} of a directory's entries as relative paths
  the ignore rules keep, with only the files `wanted-file?` accepts."
  [entries relative-directory rule-sets wanted-file?]
  (let [relative-child (fn [^js entry]
                         (if (= "" relative-directory)
                           (.-name entry)
                           (str relative-directory "/" (.-name entry))))]
    {:subdirectories (into []
                           (comp (filter #(.isDirectory ^js %))
                                 (map relative-child)
                                 (remove #(ignore/directory-ignored? rule-sets %)))
                           entries)
     :files (into []
                  (comp (filter #(.isFile ^js %))
                        (map relative-child)
                        (filter wanted-file?)
                        (remove #(ignore/ignored? rule-sets %)))
                  entries)}))

(defn walk-files
  "Relative paths under `root-directory` passing `wanted-file?`, honoring
  each .gitignore and `base-rule-sets`. For roots outside git."
  [root-directory base-rule-sets wanted-file?]
  (letfn [(walk-directory [relative-directory inherited-rule-sets]
            (let [directory (if (= "" relative-directory)
                              root-directory
                              (absolute-path root-directory relative-directory))
                  parallel-walks 8]
              (-> (task/all [(read-text (path/join directory ".gitignore")) (directory-entries directory)])
                  (task/then
                   (fn [[gitignore-text entries]]
                     (let [rule-sets (cond-> inherited-rule-sets
                                       gitignore-text (conj (ignore/rule-set-from-text relative-directory gitignore-text)))
                           {:keys [subdirectories files]} (kept-entries entries relative-directory rule-sets wanted-file?)]
                       (task/then (task/map-limited parallel-walks #(walk-directory % rule-sets) subdirectories)
                                  (fn [nested] (into files cat nested)))))))))]
    (walk-directory "" (vec base-rule-sets))))
