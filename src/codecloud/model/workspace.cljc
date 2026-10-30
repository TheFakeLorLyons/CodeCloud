(ns codecloud.model.workspace
  "Where clouds live in a workspace. Paths use forward slashes, and hosts
  fold letter case first on file systems that ignore it."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.source :refer [directory-of file-name]]))

(def project-files
  "Files that mark the root of a project, in any language the catalog
  knows, such as deps.edn, package.json, or pyproject.toml."
  catalog/project-markers)

(defn inside?
  "True when `path` lies within `directory`."
  [directory path]
  (string/starts-with? path (str directory "/")))

(defn nearest-cloud-file
  "The cloud file of `cloud-file-paths` whose folder most closely
  encloses `path`, preferring `default-name` within one folder, or nil."
  [cloud-file-paths path default-name]
  (->> cloud-file-paths
       (filter #(inside? (directory-of %) path))
       (sort-by (fn [cloud-file-path]
                  [(- (count (directory-of cloud-file-path)))
                   (not= default-name (file-name cloud-file-path))
                   cloud-file-path]))
       first))

(defn ancestor-directories
  "Directories from the one holding `path` up to and including `top`,
  nearest first; empty when `path` lies outside `top`."
  [top path]
  (if-not (inside? top path)
    []
    (loop [directory (directory-of path)
           found []]
      (if (or (= directory top) (not (inside? top directory)))
        (conj found top)
        (recur (directory-of directory) (conj found directory))))))

(defn cloud-home
  "Folder for a cloud file serving `path`, the nearest enclosing folder up
  to `top` that `project-directory?` accepts, or `top` itself."
  [top path project-directory?]
  (or (first (filter project-directory? (ancestor-directories top path))) top))

(defn focus-line
  "Line to arrive at in the cloud from an editor, the cursor's line when it
  is on screen, or else the middle of what shows."
  [cursor-line first-visible-line last-visible-line]
  (if (<= first-visible-line cursor-line last-visible-line)
    cursor-line
    (quot (+ first-visible-line last-visible-line) 2)))

(defn cloud-name
  "Name shown for a cloud: its file's name without the extension, or its
  folder's name for a file with the default name."
  [cloud-file-path default-name]
  (let [cloud-file-name (file-name cloud-file-path)
        folder (file-name (directory-of cloud-file-path))
        stem (string/replace cloud-file-name #"\.cld$" "")]
    (if (or (= cloud-file-name default-name) (string/blank? stem))
      (if (string/blank? folder) "CodeCloud" folder)
      stem)))
