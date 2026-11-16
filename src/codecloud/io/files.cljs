(ns codecloud.io.files
  "File access for the analyzer, which reads text, hashes it as git does,
  walks folders outside git, and lists what ignored folders hold."
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

(defn path-exists? [file-path]
  (.existsSync fs file-path))

(defn content-key
  "The git blob id of `text`, so a file on disk shares cache entries with
  the same blob from history."
  [text]
  (let [^js content (js/Buffer.from text "utf8")]
    (-> (.createHash crypto "sha1")
        (.update (str "blob " (.-length content) "\u0000"))
        (.update content)
        (.digest "hex"))))

(defn stamp
  "Resolves to what changes with a file's content, its times, size, and
  inode, or nil when it is not a readable file."
  [file-path]
  (-> (.stat (.-promises fs) file-path)
      (task/then (fn [^js stats]
                   (when (.isFile stats)
                     [(.-mtimeMs stats) (.-ctimeMs stats) (.-size stats) (.-ino stats)])))
      (task/recover (fn [_error] nil))))

(defn- directory-entries [directory]
  (-> (.readdir (.-promises fs) directory #js {:withFileTypes true})
      (task/then (fn [entries] (vec entries)))
      (task/recover (fn [_error] []))))

(defn- relative-child [relative-directory ^js entry]
  (if (= "" relative-directory)
    (.-name entry)
    (str relative-directory "/" (.-name entry))))

(defn- sorted-entries
  "{:subdirectories :files :ignored-subdirectories :ignored-files} of a
  directory's entries as relative paths, by whether the ignore rules keep
  them."
  [entries relative-directory rule-sets]
  (let [{subdirectories true ignored-subdirectories false}
        (group-by #(not (ignore/directory-ignored? rule-sets %))
                  (into [] (comp (filter #(.isDirectory ^js %)) (map #(relative-child relative-directory %))) entries))
        {files true ignored-files false}
        (group-by #(not (ignore/ignored? rule-sets %))
                  (into [] (comp (filter #(.isFile ^js %)) (map #(relative-child relative-directory %))) entries))]
    {:subdirectories (vec subdirectories)
     :files (vec files)
     :ignored-subdirectories (vec ignored-subdirectories)
     :ignored-files (vec ignored-files)}))

(defn walk-tree
  "Resolves to {:files :ignored-folders :ignored-files}, the relative paths
  under `root-directory` that each .gitignore and `base-rule-sets` keep,
  and the outermost folders, ending in a slash, and the files they
  ignore. For roots outside git."
  [root-directory base-rule-sets]
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
                           {:keys [subdirectories files ignored-subdirectories ignored-files]}
                           (sorted-entries entries relative-directory rule-sets)]
                       (task/then (task/map-limited parallel-walks #(walk-directory % rule-sets) subdirectories)
                                  (fn [nested]
                                    {:files (into files (mapcat :files) nested)
                                     :ignored-folders (into (mapv #(str % "/") ignored-subdirectories)
                                                            (mapcat :ignored-folders) nested)
                                     :ignored-files (into ignored-files (mapcat :ignored-files) nested)}))))))))]
    (walk-directory "" (vec base-rule-sets))))

(defn walk-files
  "Relative paths under `root-directory` passing `wanted-file?`, honoring
  each .gitignore and `base-rule-sets`. For roots outside git."
  [root-directory base-rule-sets wanted-file?]
  (task/then (walk-tree root-directory base-rule-sets)
             (fn [{:keys [files]}] (filterv wanted-file? files))))

(defn files-under
  "Resolves to the relative paths of every file inside the folder at
  `relative-folder`, however deep and whatever ignores it, leaving out
  .git folders."
  [root-directory relative-folder]
  (letfn [(walk-directory [relative-directory]
            (let [parallel-walks 8]
              (task/then (directory-entries (absolute-path root-directory relative-directory))
                         (fn [entries]
                           (let [folders (into []
                                               (comp (filter #(.isDirectory ^js %))
                                                     (remove #(= ".git" (.-name ^js %)))
                                                     (map #(relative-child relative-directory %)))
                                               entries)
                                 found (into []
                                             (comp (filter #(.isFile ^js %)) (map #(relative-child relative-directory %)))
                                             entries)]
                             (task/then (task/map-limited parallel-walks walk-directory folders)
                                        (fn [nested] (into found cat nested))))))))]
    (walk-directory relative-folder)))

(defn folder-children
  "Resolves to what the folder at `relative-folder` directly holds, as
  [{:name :folder?}], \"\" standing for `root-directory` itself."
  [root-directory relative-folder]
  (task/then (directory-entries (if (= "" relative-folder) root-directory (absolute-path root-directory relative-folder)))
             (fn [entries]
               (into []
                     (keep (fn [^js entry]
                             (cond
                               (.isDirectory entry) {:name (.-name entry) :folder? true}
                               (.isFile entry) {:name (.-name entry) :folder? false})))
                     entries))))
