(ns codecloud.model.ignored
  "What a cloud leaves out that the viewer may still choose to see, the
  folders and files that git, CodeCloud, or the cloud file ignore and the
  files of a kind CodeCloud does not read. A path chosen to be seen is a file's, or a
  folder's ending in a slash, which brings in everything inside it. Paths
  are relative to the cloud's folder, with forward slashes."
  (:require [clojure.string :as string]))

(def binary-extensions
  "Extensions of files holding no text to read as a page, which the list
  of what the cloud leaves out passes over."
  #{"7z" "a" "avif" "bin" "bmp" "bz2" "class" "db" "dll" "dmg" "dylib" "eot" "exe" "flac" "gif" "gz" "icns" "ico"
    "iso" "jar" "jpeg" "jpg" "lib" "m4a" "mov" "mp3" "mp4" "o" "obj" "ogg" "otf" "pdf" "png" "psd" "pyc" "rar"
    "so" "sqlite" "tgz" "tif" "tiff" "ttf" "vsix" "war" "wasm" "wav" "webm" "webp" "woff" "woff2" "xz" "zip"})

(defn folder-path?
  "True for a folder's path, which ends in a slash."
  [path]
  (string/ends-with? path "/"))

(defn- file-name [path]
  (peek (string/split path #"/")))

(defn readable?
  "True for a file whose name promises text that could be read as a page."
  [path]
  (let [name-of-file (file-name path)
        dot (string/last-index-of name-of-file ".")]
    (not (and dot (pos? dot)
              (contains? binary-extensions (string/lower-case (subs name-of-file (inc dot))))))))

(defn valid-path?
  "True for a relative path inside the cloud's folder, with no empty,
  \".\", or \"..\" segment, \"\" standing for the folder itself."
  [path]
  (and (string? path)
       (or (= "" path)
           (let [segments (string/split (if (folder-path? path) (subs path 0 (dec (count path))) path) #"/" -1)]
             (and (not (string/starts-with? path "/"))
                  (not (string/includes? path "\\"))
                  (every? #(not (contains? #{"" "." ".."} %)) segments))))))

(defn chosen-paths
  "The paths of `value` chosen to be seen though ignored that stay inside
  the cloud's folder, or none when it is not a list of them."
  [value]
  (if (sequential? value)
    (filterv #(and (valid-path? %) (not= "" %)) value)
    []))

(defn shown-predicate
  "Predicate true for the paths that `shown-paths` bring into the cloud."
  [shown-paths]
  (let [folders (filterv folder-path? shown-paths)
        files (set (remove folder-path? shown-paths))]
    (fn [path]
      (boolean (or (contains? files path)
                   (some #(string/starts-with? path %) folders))))))

(defn shown-by
  "How `path` is shown among `shown-paths`, :folder when a folder holding
  it was chosen, which settles it, else :itself when it was chosen, or
  nil."
  [shown-paths path]
  (cond
    (some #(and (folder-path? %) (not= % path) (string/starts-with? path %)) shown-paths) :folder
    (some #{path} shown-paths) :itself))

(defn toggled
  "`shown-paths` with `path` chosen, or no longer chosen if it was."
  [shown-paths path]
  (if (some #{path} shown-paths)
    (filterv #(not= path %) shown-paths)
    (conj (vec shown-paths) path)))

(defn- outermost-folder
  "The outermost folder holding `path` that `folder-ignored?` says is
  ignored, as a path ending in a slash, or nil."
  [folder-ignored? path]
  (let [segments (string/split path #"/")]
    (some (fn [depth]
            (let [folder (string/join "/" (take depth segments))]
              (when (folder-ignored? folder) (str folder "/"))))
          (range 1 (count segments)))))

(defn- inside-any? [folders path]
  (some #(and (not= % path) (string/starts-with? path %)) folders))

(defn sorted-entries
  "`entries` with folders first, then files, each by path."
  [entries]
  (vec (sort-by (juxt (comp not :folder?) :path) entries)))

(defn entries
  "What the cloud leaves out of its folder, as entries {:path :folder?
  :reason}, folders first. The `git-ignored` paths, folders ending in a
  slash, come as :git, the `files` that `cloud-ignored?` removes come as
  :cloud, under the outermost folder `cloud-folder-ignored?` ignores when
  there is one, and the files left that `read?` turns away come as :type.
  Entries inside a folder listed come with that folder instead, and the
  repository's own .git folder and files holding no text never show."
  [{:keys [git-ignored files cloud-ignored? cloud-folder-ignored? read?]}]
  (let [git-entries (keep (fn [path]
                            (cond
                              (folder-path? path) {:path path :folder? true :reason :git}
                              (readable? path) {:path path :folder? false :reason :git}))
                          git-ignored)
        file-entries (keep (fn [path]
                             (cond
                               (cloud-ignored? path) (if-let [folder (outermost-folder cloud-folder-ignored? path)]
                                                       {:path folder :folder? true :reason :cloud}
                                                       (when (readable? path) {:path path :folder? false :reason :cloud}))
                               (and (not (read? path)) (readable? path)) {:path path :folder? false :reason :type}))
                           files)
        unique (vals (reduce (fn [by-path {:keys [path] :as entry}]
                               (if (contains? by-path path) by-path (assoc by-path path entry)))
                             {}
                             (concat git-entries file-entries)))
        listed-folders (filterv folder-path? (map :path unique))]
    (sorted-entries (remove (fn [{:keys [path]}]
                              (or (= ".git/" path)
                                  (string/starts-with? path ".git/")
                                  (inside-any? listed-folders path)))
                            unique))))

(defn child-entries
  "Entries for what `folder`, a path ending in a slash, directly holds,
  given as `children` {:name :folder?}, leaving out .git folders and files
  holding no text."
  [folder children]
  (sorted-entries (keep (fn [{child-name :name :keys [folder?]}]
                          (cond
                            (= ".git" child-name) nil
                            folder? {:path (str folder child-name "/") :folder? true}
                            (readable? child-name) {:path (str folder child-name) :folder? false}))
                        children)))

(defn limited
  "{:entries :more}, at most `most` of `entries` and how many more there were."
  [entries most]
  {:entries (vec (take most entries))
   :more (max 0 (- (count entries) most))})
