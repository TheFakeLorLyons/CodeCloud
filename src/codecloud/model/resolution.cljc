(ns codecloud.model.resolution
  "Resolves imports to pages once every file is known, by each language's
  rules. Imports reaching no file stay external requirements."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.graph :as graph]
            [codecloud.model.source :as source :refer [directory-of]]))

;; Paths

(defn joined
  "The path `relative` reaches from `directory`, with . and .. followed,
  or nil when it climbs above the root."
  [directory relative]
  (loop [remaining (concat (when-not (string/blank? directory) (string/split directory #"/"))
                           (string/split relative #"/"))
         result []]
    (if-let [part (first remaining)]
      (case part
        ("" ".") (recur (rest remaining) result)
        ".." (when (seq result) (recur (rest remaining) (pop result)))
        (recur (rest remaining) (conj result part)))
      (string/join "/" result))))

(defn- ancestor-directories
  "The folders holding `path`, nearest first, ending with the root."
  [path]
  (loop [directory (directory-of path)
         found []]
    (if (string/blank? directory)
      (conj found "")
      (recur (directory-of directory) (conj found directory)))))

(defn- first-present
  "The first of `candidates` that is a file of the cloud."
  [paths candidates]
  (some #(when (and % (contains? paths %)) %) candidates))

;; The project index

(defn- python-roots
  "Folders whose packages Python imports by absolute name: the root, src
  and lib, and the folder of each Python project with its own src."
  [paths manifests]
  (let [project-directories (into (sorted-set)
                                  (comp (filter #(re-find #"(?:^|/)(?:pyproject\.toml|setup\.cfg|setup\.py)$" %))
                                        (map directory-of))
                                  (concat paths (map :path manifests)))]
    (vec (distinct (concat ["" "src" "lib"]
                           (mapcat (fn [directory] [directory (joined directory "src")]) project-directories))))))

(defn- script-configs
  "Each tsconfig.json or jsconfig.json as {:directory :base :aliases},
  deepest first."
  [manifests]
  (->> manifests
       (filter #(= :script-config (get-in % [:manifest :kind])))
       (map (fn [{:keys [path manifest]}]
              (let [directory (directory-of path)
                    base (joined directory (or (:base-url manifest) "."))]
                {:directory directory
                 :base (when (:base-url manifest) base)
                 :aliases (for [[pattern targets] (:paths manifest)
                                :when (and (string? pattern) (sequential? targets))]
                            [pattern (filterv string? targets) (or base directory)])})))
       (sort-by (comp - count :directory))
       vec))

(defn- packages
  "Map of each package.json's package name to {:directory :main}."
  [manifests]
  (into {}
        (keep (fn [{:keys [path manifest]}]
                (when (and (= :package (:kind manifest)) (string? (:name manifest)))
                  [(:name manifest) {:directory (directory-of path) :main (:main manifest)}])))
        manifests))

(defn- absolute-folder-in-cloud
  "The folder of the cloud an absolute path names: the longest run of its
  last folders that is one, so a build file written on another machine
  still points home."
  [folders absolute]
  (let [segments (remove string/blank? (string/split absolute #"/"))]
    (some (fn [dropped]
            (let [candidate (string/join "/" (drop dropped segments))]
              (when (contains? folders candidate) candidate)))
          (range (count segments)))))

(defn- include-folders
  "The folders #include searches, as folders of the cloud: those the cloud
  file lists under :include-paths, then those C and C++ build files name."
  [paths manifests include-paths]
  (let [folders (into #{} (mapcat ancestor-directories) paths)
        from-manifests (for [{:keys [path manifest]} manifests
                             :when (= :c-build (:kind manifest))
                             {:keys [folder base]} (:include-folders manifest)]
                         (case base
                           :absolute (absolute-folder-in-cloud folders folder)
                           :root (joined "" folder)
                           (joined (directory-of path) folder)))]
    (vec (distinct (filter #(contains? folders %)
                           (concat (map #(joined "" %) include-paths) from-manifests))))))

(defn project-index
  "What resolvers need about the cloud: :paths, :unit-of, :language-of,
  :units, :definition-names of each unit, :python-roots, :script-configs,
  :packages, :include-folders, and :paths-by-file-name."
  ([file-facts manifests] (project-index file-facts manifests {}))
  ([file-facts manifests {:keys [include-paths]}]
   (let [paths (into #{} (map :path) file-facts)]
     {:paths paths
      :unit-of (into {} (map (juxt :path graph/node-id)) file-facts)
      :language-of (into {} (map (juxt :path #(:language % :clojure))) file-facts)
      :units (into #{} (map graph/node-id) file-facts)
      :definition-names (into {}
                              (map (fn [[unit facts-list]] [unit (into [] (comp (mapcat :definitions) (map :name)) facts-list)]))
                              (group-by graph/node-id file-facts))
      :python-roots (python-roots paths manifests)
      :script-configs (script-configs manifests)
      :packages (packages manifests)
      :include-folders (include-folders paths manifests include-paths)
      :paths-by-file-name (group-by source/file-name paths)})))

;; Resolvers, each from an import to the paths it reaches

(defn- python-module-path
  "The file of the dotted module `module-name` under `root`, a module
  file or a package's __init__.py."
  [paths root module-name]
  (let [base (joined root (string/replace module-name "." "/"))]
    (when base
      (first-present paths [(str base ".py") (str base "/__init__.py") (str base ".pyi") (str base "/__init__.pyi")]))))

(defn- python-base
  "[roots module] for a Python specifier: a relative import's package
  folder, or the roots an absolute import searches."
  [{:keys [python-roots] :as index} path specifier]
  (let [dots (count (re-find #"^\.*" specifier))
        module-name (subs specifier dots)]
    (if (pos? dots)
      [[(nth (iterate directory-of (directory-of path)) (dec dots))] module-name]
      [(distinct (cond->> python-roots
                   (not (contains? (:paths index) (joined (directory-of path) "__init__.py")))
                   (cons (directory-of path))))
       module-name])))

(defn- python-targets
  "[{:path :names}] for a Python import: each submodule a from-import
  names, and the module itself for names that are not submodules."
  [{:keys [paths] :as index} path {:keys [specifier names]}]
  (let [[roots module-name] (python-base index path specifier)
        module-at (fn [dotted-name]
                    (if (string/blank? dotted-name)
                      (some #(first-present paths [(joined % "__init__.py")]) roots)
                      (some #(python-module-path paths % dotted-name) roots)))
        submodule-name (fn [imported] (if (string/blank? module-name) imported (str module-name "." imported)))
        submodules (into {}
                         (keep (fn [imported]
                                 (when (not= "*" imported)
                                   (when-let [found (module-at (submodule-name imported))]
                                     [imported found]))))
                         names)
        remaining-names (remove #(contains? submodules %) names)
        module-path (when (or (empty? names) (seq remaining-names)) (module-at module-name))]
    (cond-> (mapv (fn [[imported found]] {:path found :names [imported] :module? true}) submodules)
      module-path (conj (cond-> {:path module-path}
                          (seq names) (assoc :names (vec remaining-names)))))))

(def ^:private script-extensions
  [".ts" ".tsx" ".js" ".jsx" ".mjs" ".cjs" ".mts" ".cts" ".d.ts"])

(defn- probe-script
  "The file a script import reaches: the path itself, with an extension,
  an index file, or a .ts file written as .js."
  [paths candidate]
  (when candidate
    (or (first-present paths [candidate])
        (first-present paths (map #(str candidate %) script-extensions))
        (first-present paths (map #(str candidate "/index" %) script-extensions))
        (when-let [[_ stem extension] (re-find #"^(.*)\.(js|jsx|mjs|cjs)$" candidate)]
          (first-present paths (map #(str stem %) (case extension
                                                    "js" [".ts" ".tsx"]
                                                    "jsx" [".tsx"]
                                                    "mjs" [".mts"]
                                                    "cjs" [".cts"])))))))

(defn- alias-match
  "What the * of a paths pattern such as @/* stands for in `specifier`,
  or nil when the pattern does not match."
  [pattern specifier]
  (if-let [star (string/index-of pattern "*")]
    (let [prefix (subs pattern 0 star)
          suffix (subs pattern (inc star))]
      (when (and (string/starts-with? specifier prefix)
                 (string/ends-with? specifier suffix)
                 (>= (count specifier) (+ (count prefix) (count suffix))))
        (subs specifier (count prefix) (- (count specifier) (count suffix)))))
    (when (= pattern specifier) "")))

(defn- bare-script-target
  "The file a bare specifier reaches through path aliases, base folders,
  or the cloud's packages."
  [{:keys [paths script-configs packages]} path specifier]
  (let [covering (filter #(or (string/blank? (:directory %))
                              (string/starts-with? path (str (:directory %) "/")))
                         script-configs)]
    (or (some (fn [{:keys [aliases base]}]
                (or (some (fn [[pattern targets alias-base]]
                            (when-let [matched (alias-match pattern specifier)]
                              (some #(probe-script paths (joined alias-base (string/replace % "*" matched))) targets)))
                          aliases)
                    (when base (probe-script paths (joined base specifier)))))
              covering)
        (some (fn [[package-name {:keys [directory main]}]]
                (cond
                  (= specifier package-name)
                  (or (when main (probe-script paths (joined directory main)))
                      (probe-script paths (joined directory "index")))
                  (string/starts-with? specifier (str package-name "/"))
                  (probe-script paths (joined directory (subs specifier (inc (count package-name)))))))
              packages))))

(defn- script-target
  "The file a JavaScript or TypeScript import reaches."
  [{:keys [paths] :as index} path specifier]
  (cond
    (or (string/starts-with? specifier "./") (string/starts-with? specifier "../") (= "." specifier) (= ".." specifier))
    (probe-script paths (joined (directory-of path) specifier))
    (string/starts-with? specifier "/") (probe-script paths (joined "" specifier))
    :else (bare-script-target index path specifier)))

(defn- style-sheet-target
  "The sheet a CSS @import or Sass @use reaches, trying the partials and
  index files Sass looks for."
  [{:keys [paths]} path specifier]
  (when-not (re-find #"^(?:[A-Za-z][A-Za-z0-9+.-]*:|//|~)" specifier)
    (let [candidate (if (string/starts-with? specifier "/")
                      (joined "" specifier)
                      (joined (directory-of path) specifier))]
      (when candidate
        (let [folder (directory-of candidate)
              stem (source/file-name candidate)
              in-folder (fn [file-name] (joined folder file-name))]
          (first-present paths
                         (concat [candidate]
                                 (for [prefix ["" "_"]
                                       extension [".scss" ".sass" ".css" ".less"]]
                                   (in-folder (str prefix stem extension)))
                                 [(str candidate "/_index.scss") (str candidate "/index.scss")
                                  (str candidate "/_index.sass")])))))))

(defn- page-target
  "The file an HTML address reaches. A root-relative address tries each
  folder above the page, nearest first."
  [{:keys [paths]} path specifier]
  (let [candidates (if (string/starts-with? specifier "/")
                     (map #(joined % specifier) (ancestor-directories path))
                     [(joined (directory-of path) specifier)])]
    (first-present paths (mapcat (fn [candidate]
                                   (when candidate
                                     [candidate (str candidate "/index.html") (str candidate ".html")]))
                                 candidates))))

(defn- command-path-target
  "The file a shell command names, from the script's folder or the root."
  [{:keys [paths]} path specifier]
  (let [[_ after-variable] (re-find #"^(?:\$\{?[A-Za-z_][A-Za-z0-9_]*\}?|\$\(.*?\))/(.*)$" specifier)]
    (cond
      after-variable (first-present paths [(joined (directory-of path) after-variable)])
      (re-find #"^[$~/]" specifier) nil
      :else (first-present paths [(joined (directory-of path) specifier) (joined "" specifier)]))))

(defn- java-class-target
  "The Java or Kotlin file that declares the class `class-name`, matched
  by its package folders at the end of a path, when exactly one matches."
  [{:keys [paths]} class-name]
  (let [outer-class (first (string/split class-name #"\$"))
        relative (string/replace outer-class "." "/")
        endings (set (for [extension [".java" ".kt"]] (str relative extension)))
        matches (filter (fn [candidate]
                          (some #(or (= candidate %) (string/ends-with? candidate (str "/" %))) endings))
                        paths)]
    (when (= 1 (count matches)) (first matches))))

(defn- shared-folder-count
  "How many leading folders two paths share."
  [path other]
  (count (take-while true? (map = (string/split (directory-of path) #"/") (string/split (directory-of other) #"/")))))

(defn- nearest-ending
  "The file of the cloud whose path ends with `specifier`: the only one,
  else the one sharing the most folders with `path`, or nil on a tie."
  [paths-by-file-name path specifier]
  (let [tail (string/replace specifier #"^(?:\.{1,2}/)+" "")
        candidates (filter #(or (= % tail) (string/ends-with? % (str "/" tail)))
                           (get paths-by-file-name (source/file-name tail)))]
    (if (<= (count candidates) 1)
      (first candidates)
      (let [[best runner-up] (sort-by (comp - first) (map (juxt #(shared-folder-count path %) identity) candidates))]
        (when (> (first best) (first runner-up)) (second best))))))

(defn- include-target
  "The file an #include reaches: beside the including file for a quoted
  name, then in each include folder, then the file whose path ends with
  the name."
  [{:keys [paths include-folders paths-by-file-name]} path {:keys [specifier system?]}]
  (or (when-not system? (first-present paths [(joined (directory-of path) specifier)]))
      (first-present paths (map #(joined % specifier) include-folders))
      (nearest-ending paths-by-file-name path specifier)))

(defn- targets
  "[{:path :names :module?} or {:unit}] for one import of the file at
  `path`."
  [index path {:keys [resolver specifier] :as import}]
  (let [found-path (fn [target-path] (when target-path [{:path target-path}]))]
    (case resolver
      :python (python-targets index path import)
      :python-module (found-path (some #(python-module-path (:paths index) % specifier) (:python-roots index)))
      :javascript (found-path (script-target index path specifier))
      :css (found-path (style-sheet-target index path specifier))
      :html (found-path (page-target index path specifier))
      :path (found-path (command-path-target index path specifier))
      :clojure-namespace (when (contains? (:units index) specifier) [{:unit specifier}])
      :java-class (found-path (java-class-target index specifier))
      :c-include (found-path (include-target index path import))
      nil)))

;; Requirements

(defn- loading-platform
  "The language family whose cycles an import counts toward, or nil when
  its cycles do not matter."
  [kind source-language target-language]
  (let [family (catalog/language-family source-language)]
    (when (and (contains? #{:import :source} kind)
               (= family (catalog/language-family target-language))
               (not= :allowed (catalog/cycle-severity source-language)))
      family)))

(defn- binds-for
  "Bindings of a from-import that reach a target: a submodule binds as a
  module, other names as themselves."
  [{:keys [binds]} {:keys [names module?]}]
  (if names
    (into [] (keep (fn [bind]
                     (when (some #(= % (:name bind)) names)
                       (if module? (assoc bind :name nil) bind))))
          binds)
    (vec binds)))

(defn- constructor-name?
  "True for a C++ constructor or destructor, named after its own class,
  whose uses are the class's."
  [segments]
  (let [own (peek segments)]
    (and (> (count segments) 1)
         (or (string/starts-with? own "~") (= own (peek (pop segments)))))))

(defn- included-binds
  "One bind holding the uses of `unit`'s definitions among the names an
  including file writes, a qualified name matched by its last part."
  [{:keys [definition-names]} unit identifier-counts]
  (let [uses (into {}
                   (keep (fn [definition-name]
                           (let [segments (string/split definition-name #"::")
                                 use-count (get identifier-counts (peek segments) 0)]
                             (when (and (pos? use-count) (not (constructor-name? segments)))
                               [definition-name use-count]))))
                   (distinct (get definition-names unit)))]
    (if (seq uses) [{:local nil :name nil :uses uses}] [])))

(defn- import-requirements
  "Requirements for one import: one per target it reaches, or an external
  one when it reaches none and its adapter keeps misses. A file that
  counts the names it writes, as C does, binds what its targets define."
  [{:keys [unit-of language-of] :as index} path identifier-counts import]
  (let [source-language (get language-of path)
        found (remove #(= path (:path %)) (targets index path import))]
    (if (seq found)
      (mapv (fn [{target-path :path unit :unit :as target}]
              (let [target-language (if target-path (get language-of target-path) :clojure)
                    target-unit (or unit (get unit-of target-path))
                    kind (if (and (= :import (:kind import)) (= :css target-language) (not= :css source-language))
                           :style
                           (:kind import))]
                {:target target-unit
                 :kind kind
                 :platform (loading-platform kind source-language target-language)
                 :specifier (:specifier import)
                 :line (:line import)
                 :binds (if identifier-counts
                          (included-binds index target-unit identifier-counts)
                          (binds-for import target))}))
            found)
      (if (:drop-unresolved? import)
        []
        [{:target (:specifier import) :kind (:kind import) :external? true :specifier (:specifier import)
          :line (:line import)}]))))

(defn- bind-references
  "One bind's uses by definition name, with uses of a module alone under
  nil."
  [{imported-name :name :keys [uses]}]
  (if (and imported-name (not (contains? #{"*" "default"} imported-name)))
    {imported-name (reduce + 0 (vals uses))}
    (or uses {})))

(defn- requirement-references
  "{target {definition-name use-count}} for the binds of a file's
  requirements that reach a page of the cloud."
  [requirements]
  (reduce (fn [references {:keys [target binds external?]}]
            (if (or external? (empty? binds))
              references
              (update references target #(merge-with + % (apply merge-with + (map bind-references binds))))))
          {}
          requirements))

(defn- resolve-file
  "`facts` with imports resolved into requirements and
  :resolved-references. A Clojure string require that reaches the cloud
  gives way to its interop edge."
  [index {:keys [path imports identifier-counts] :as facts}]
  (if (empty? imports)
    facts
    (let [resolved (mapv (fn [import] [import (import-requirements index path identifier-counts import)]) imports)
          replaced (into #{}
                         (keep (fn [[import requirements]]
                                 (when (and (:replaces-requirement import) (seq requirements))
                                   (:replaces-requirement import))))
                         resolved)
          added (into [] (mapcat second) resolved)
          references (requirement-references added)]
      (cond-> (assoc facts :requirements (into (filterv #(not (and (:js-module? %) (contains? replaced (:target %))))
                                                        (:requirements facts))
                                               added))
        (seq references) (assoc :resolved-references references)))))

(defn resolve-imports
  "File facts with every import turned into requirements, given the
  manifests read beside them and the cloud file's :include-paths."
  ([file-facts manifests] (resolve-imports file-facts manifests {}))
  ([file-facts manifests options]
   (let [index (project-index file-facts manifests options)]
     (mapv #(resolve-file index %) file-facts))))
