(ns codecloud.languages.manifests
  "Project files guiding import resolution, never pages: package.json,
  tsconfig.json, jsconfig.json, pyproject.toml, and setup.cfg, and for C
  and C++ the include folders that compile_commands.json,
  compile_flags.txt, CMakeLists.txt, Makefiles, and meson.build name."
  (:require [clojure.string :as string]
            [codecloud.languages.json :as json]))

(def file-names
  "Names of the files read as manifests."
  #{"package.json" "tsconfig.json" "jsconfig.json" "pyproject.toml" "setup.cfg"
    "compile_commands.json" "compile_flags.txt" "CMakeLists.txt" "Makefile" "makefile" "GNUmakefile"
    "meson.build"})

;; C and C++ include folders, each {:folder :base}, where :base is
;; :manifest for a folder relative to the build file, :root for one
;; relative to the cloud's root, and :absolute for a file system path.

(defn- collapsed
  "An absolute path with its . segments and each folder/.. pair folded
  away."
  [path]
  (let [root (re-find #"^(?:/|[A-Za-z]:/)" path)
        segments (reduce (fn [kept segment]
                           (cond
                             (contains? #{"" "."} segment) kept
                             (and (= ".." segment) (seq kept) (not= ".." (peek kept))) (pop kept)
                             :else (conj kept segment)))
                         []
                         (string/split (subs path (count root)) #"/"))]
    (str root (string/join "/" segments))))

(defn- folder-entry
  "{:folder :base} for a folder a build file names, :absolute when it
  starts at a file system root, else relative to `base`."
  [folder base]
  (let [forward (string/replace folder "\\" "/")]
    (if (re-find #"^(?:/|[A-Za-z]:/)" forward)
      {:folder (collapsed forward) :base :absolute}
      {:folder forward :base base})))

(defn- argument-folders
  "Folders that compiler arguments add to the include search with -I,
  -iquote, -isystem, -idirafter, or /I, joined to the flag or after it."
  [arguments]
  (loop [remaining arguments
         found []]
    (if-let [argument (first remaining)]
      (if-let [[_ joined] (re-matches #"(?:-I|-iquote|-isystem|-idirafter|/I)(.*)" argument)]
        (if (string/blank? joined)
          (recur (drop 2 remaining) (cond-> found (second remaining) (conj (folder-entry (second remaining) :manifest))))
          (recur (rest remaining) (conj found (folder-entry joined :manifest))))
        (recur (rest remaining) found))
      found)))

(defn- json-unescaped
  "A JSON string's text with escaped quotes restored and escaped
  backslashes read as the path separators they are."
  [text]
  (-> text
      (string/replace "\\\\" "/")
      (string/replace "\\\"" "\"")))

(defn- compile-commands-folders
  "Include folders in compile_commands.json, a relative one joined to its
  command's directory. Read with patterns, since the file can run to
  megabytes."
  [text]
  (distinct
   (for [entry (re-seq #"\{[^{}]*\}" text)
         :let [directory (some-> (re-find #"\"directory\"\s*:\s*\"((?:[^\"\\]|\\.)*)\"" entry) second json-unescaped)]
         [_ written] (re-seq #"(?:^|[\s\"])(?:-I|-iquote|-isystem|-idirafter|/I)(?:\"\s*,\s*\"|\s+)?([^\s\",\[\]]+)" entry)
         :let [folder (folder-entry (json-unescaped written) :manifest)]]
     (if (and directory (not= :absolute (:base folder)))
       (folder-entry (str directory "/" (:folder folder)) :manifest)
       folder))))

(def ^:private cmake-keywords
  "Words that CMake's include commands take besides folders."
  #{"SYSTEM" "BEFORE" "AFTER" "PUBLIC" "PRIVATE" "INTERFACE"})

(defn- cmake-folder
  "The folder a CMake argument names, unwrapping $<BUILD_INTERFACE:...>
  and the source directory variables, or nil when another variable or
  generator expression stands in it."
  [argument]
  (let [unwrapped (or (second (re-matches #"\$<BUILD_INTERFACE:(.*)>" argument)) argument)
        [_ variable remainder] (re-matches #"\$\{(CMAKE_CURRENT_SOURCE_DIR|CMAKE_CURRENT_LIST_DIR|PROJECT_SOURCE_DIR|CMAKE_SOURCE_DIR)\}/?(.*)"
                                           unwrapped)]
    (cond
      variable (folder-entry (if (string/blank? remainder) "." remainder)
                             (if (contains? #{"PROJECT_SOURCE_DIR" "CMAKE_SOURCE_DIR"} variable) :root :manifest))
      (string/includes? unwrapped "$") nil
      :else (folder-entry unwrapped :manifest))))

(defn- cmake-include-folders
  "Folders that include_directories and target_include_directories name,
  the first word of the second being its target."
  [text]
  (let [uncommented (string/replace text #"(?m)#.*$" "")]
    (for [[_ command arguments] (re-seq #"(?i)\b(include_directories|target_include_directories)\s*\(([^)]*)\)" uncommented)
          :let [words (map #(string/replace % #"^\"|\"$" "") (re-seq #"\"[^\"]*\"|[^\s\"]+" arguments))
                folders (if (= "target_include_directories" (string/lower-case command)) (rest words) words)]
          folder folders
          :when (not (contains? cmake-keywords (string/upper-case folder)))
          :let [entry (cmake-folder folder)]
          :when entry]
      entry)))

(defn- meson-include-folders
  "Folders that meson's include_directories names in quotes."
  [text]
  (for [[_ arguments] (re-seq #"include_directories\s*\(([^)]*)\)" text)
        [_ single-quoted double-quoted] (re-seq #"'([^']*)'|\"([^\"]*)\"" arguments)]
    (folder-entry (or single-quoted double-quoted) :manifest)))

(defn- c-build
  "A manifest of a C or C++ build: the include folders it names."
  [folders]
  {:kind :c-build :include-folders (vec (distinct folders))})

(defn read-manifest
  "Facts of a manifest at `path` named `file-name`, as {:path :manifest},
  where :manifest holds the :kind and what resolution needs of it."
  [path file-name text]
  {:path path
   :manifest (case file-name
               "package.json" (let [package (json/read-json text)]
                                {:kind :package
                                 :name (when (map? package) (get package "name"))
                                 :main (when (map? package) (or (get package "module") (get package "main")))})
               ("tsconfig.json" "jsconfig.json") (let [config (json/read-json text)
                                                       options (when (map? config) (get config "compilerOptions"))]
                                                   {:kind :script-config
                                                    :base-url (when (map? options) (get options "baseUrl"))
                                                    :paths (when (map? options) (get options "paths"))})
               "compile_commands.json" (c-build (compile-commands-folders text))
               "compile_flags.txt" (c-build (argument-folders (remove string/blank? (map string/trim (string/split-lines text)))))
               "CMakeLists.txt" (c-build (cmake-include-folders text))
               ("Makefile" "makefile" "GNUmakefile") (c-build (remove #(string/includes? (:folder %) "$")
                                                                      (argument-folders (re-seq #"\S+" text))))
               "meson.build" (c-build (meson-include-folders text))
               {:kind :python-project})})
