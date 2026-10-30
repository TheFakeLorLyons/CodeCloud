(ns codecloud.io.analyzer
  "Analyzes a cloud's root as its working tree or at any commit. Facts are
  cached by path and content, so only changed files parse again."
  (:require [codecloud.io.files :as files]
            [codecloud.io.git :as git]
            [codecloud.io.task :as task]
            [codecloud.io.tree-sitter :as tree-sitter]
            [codecloud.languages :as languages]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.config :as config]
            [codecloud.model.ignore :as ignore]
            [codecloud.model.selection :as selection]
            [codecloud.model.snapshot :as snapshot]
            [codecloud.model.source :as source]))

(def working-tree
  "Snapshot id of the files as they are on disk."
  :working-tree)

(def file-limit
  "Most source files one snapshot analyzes, so the editor stays
  responsive. Larger trees are cut short with a notice."
  6000)

(defn create-analyzer
  "Analysis state for the directory `root-directory`, reading C and C++
  with the grammars in `grammar-directory` when given."
  [{:keys [root-directory git-executable grammar-directory]}]
  {:root-directory root-directory
   :git-executable (or git-executable "git")
   :grammar-directory grammar-directory
   :repository-root (atom ::unknown)
   :facts-by-key (atom {})
   :snapshots (atom {:order [] :by-id {}})})

(defn repository-root
  "Resolves to the enclosing git repository's top-level directory, or nil.
  The answer is cached for the analyzer's lifetime."
  [{:keys [git-executable root-directory] :as analyzer}]
  (let [known @(:repository-root analyzer)]
    (if (not= ::unknown known)
      (task/resolved known)
      (task/then (git/repository-root git-executable root-directory)
                 (fn [found-root]
                   (reset! (:repository-root analyzer) found-root)
                   found-root)))))

(defn read-configuration
  "Resolves to {:config :problems}: the defaults, then deps.edn's
  :codecloud/config, then the cloud file's text, each overriding the last."
  [analyzer cloud-file-text]
  (task/then (files/read-text (files/absolute-path (:root-directory analyzer) "deps.edn"))
             (fn [deps-text]
               (let [from-deps (if deps-text
                                 (config/deps-edn-config deps-text)
                                 {:config {} :problems []})
                     from-cloud-file (config/cloud-file-config (or cloud-file-text ""))]
                 {:config (config/combine (:config from-deps) (:config from-cloud-file))
                  :problems (into (:problems from-deps) (:problems from-cloud-file))}))))

(defn- configured-rule-sets [configuration]
  [(ignore/rule-set "" (concat config/always-ignored (:ignore configuration)))])

(defn wanted-path-predicate
  "Predicate for the paths a configuration analyzes, by name, :include,
  and :ignore. Git's own ignore rules apply separately."
  [configuration]
  (let [scope (selection/compile-selection {:include (:include configuration)
                                            :ignore (concat config/always-ignored (:ignore configuration))})
        source-path? (config/source-path-predicate configuration)]
    (fn [path]
      (and (source-path? path)
           (selection/selected? scope path)))))

(defn- measuring-options
  "The parts of a configuration that change what a file's facts measure."
  [configuration]
  (select-keys configuration [:flags :complection]))

(defn- facts-key [path content-key measuring]
  [path content-key measuring])

(defn- parser-for
  "Resolves to {:parse function} when any of `paths` is read through a
  syntax tree, {:error message} when the grammars fail to load, and {}
  when no file needs them or the analyzer has none."
  [{:keys [grammar-directory]} paths]
  (if (and grammar-directory (some #(:syntax-tree? (catalog/language-by-name %)) paths))
    (tree-sitter/load-parser grammar-directory)
    (task/resolved {})))

(defn- parser-notice [error]
  (when error
    (str "C and C++ pages show includes without definitions, since their grammars did not load: " error)))

(defn- cached-facts
  "Facts remembered for this path and content, ::skipped for a file read
  before and left out, or nil for one never read."
  [analyzer path content-key measuring]
  (get @(:facts-by-key analyzer) (facts-key path content-key measuring)))

(defn- facts-for
  "Facts for one file, or nil when left out, cached by path, content, and
  options. `parse` reads syntax trees, when loaded. The cache empties past
  a limit."
  [analyzer path content-key text measuring parse]
  (let [cache-limit 40000
        cached (cached-facts analyzer path content-key measuring)]
    (if (some? cached)
      (when-not (= ::skipped cached) cached)
      (let [facts (some-> (languages/analyze-file path text (assoc measuring :parse-tree parse))
                          (assoc :content-key content-key))]
        (when (> (count @(:facts-by-key analyzer)) cache-limit)
          (reset! (:facts-by-key analyzer) {}))
        (swap! (:facts-by-key analyzer) assoc (facts-key path content-key measuring) (or facts ::skipped))
        facts))))

(defn- wanted-facts
  "The facts of `facts-list` that exist and are manifests or in a
  language the configuration reads."
  [configuration facts-list]
  (filterv #(and % (or (:manifest %) (config/language-wanted? configuration (:language %)))) facts-list))

(defn- remember-snapshot!
  "Keeps recent commit snapshots so scrubbing back and forth through
  history does not rebuild them."
  [analyzer built-snapshot]
  (let [snapshot-limit 80]
    (swap! (:snapshots analyzer)
           (fn [{:keys [order by-id]}]
             (let [order (conj (vec (remove #(= % (:id built-snapshot)) order)) (:id built-snapshot))
                   evicted (drop-last snapshot-limit order)]
               {:order (vec (take-last snapshot-limit order))
                :by-id (apply dissoc (assoc by-id (:id built-snapshot) built-snapshot) evicted)})))
    built-snapshot))

(defn forget-snapshots!
  "Drops remembered snapshots, for example after the configuration changed."
  [analyzer]
  (reset! (:snapshots analyzer) {:order [] :by-id {}}))

(defn remembered-snapshot
  "The commit snapshot for `sha` when it is still remembered, or nil."
  [analyzer sha]
  (get-in @(:snapshots analyzer) [:by-id sha]))

(defn- limit-notice [skipped-count]
  (when (pos? skipped-count)
    (str "Read the first " file-limit " source files and skipped " skipped-count
         " more. Folders under :ignore in the cloud file focus it.")))

(defn- working-tree-paths [analyzer configuration]
  (let [{:keys [git-executable root-directory]} analyzer
        wanted-file? (wanted-path-predicate configuration)]
    (task/then (repository-root analyzer)
               (fn [repository-root-path]
                 (if repository-root-path
                   (task/then (git/working-tree-files git-executable root-directory)
                              (fn [paths] (filterv wanted-file? paths)))
                   (files/walk-files root-directory (configured-rule-sets configuration) wanted-file?))))))

(defn- working-text
  "Resolves to the text of the working tree file at `path`: what
  `open-text` holds for it, as for a file with unsaved changes, or else
  what is on disk, or nil when unreadable."
  [analyzer open-text path]
  (if-let [text (open-text path)]
    (task/resolved text)
    (files/read-text (files/absolute-path (:root-directory analyzer) path))))

(defn- working-tree-facts
  "Resolves to the facts of each of `paths`, nil where unreadable,
  reporting progress to `on-progress`."
  [analyzer configuration paths on-progress open-text parse]
  (let [total (count paths)
        done-count (atom 0)
        progress-interval 40
        parallel-reads 16
        read-facts (fn [path]
                     (task/then (working-text analyzer open-text path)
                                (fn [text]
                                  (let [finished (swap! done-count inc)]
                                    (when (and on-progress (zero? (mod finished progress-interval)))
                                      (on-progress {:done finished :total total})))
                                  (when text
                                    (facts-for analyzer path (files/content-key text) text
                                               (measuring-options configuration) parse)))))]
    (task/map-limited parallel-reads read-facts paths)))

(defn analyze-working-tree
  "Resolves to {:snapshot :notices} for the working tree. `on-progress`,
  when given, receives {:done :total} while files are read, and
  `open-text`, from a relative path to text or nil, gives the text of
  files open with unsaved changes."
  [analyzer configuration {:keys [on-progress open-text]}]
  (task/then
   (working-tree-paths analyzer configuration)
   (fn [paths]
     (let [analyzed-paths (vec (take file-limit (sort paths)))]
       (task/then
        (parser-for analyzer analyzed-paths)
        (fn [{:keys [parse error]}]
          (task/then (working-tree-facts analyzer configuration analyzed-paths on-progress (or open-text (constantly nil))
                                         parse)
                     (fn [facts-list]
                       {:snapshot (snapshot/build-snapshot (wanted-facts configuration facts-list)
                                                           {:id working-tree
                                                            :label "Working tree"
                                                            :time (js/Date.now)
                                                            :weights (:weights configuration)
                                                            :include-paths (:include-paths configuration)})
                        :notices (filterv some? [(limit-notice (- (count paths) (count analyzed-paths)))
                                                 (parser-notice error)])}))))))))

(defn- commit-facts
  "Resolves to the facts of each of `entries`, {:path :blob}, from the
  cache, or parsed from blobs read out of git in one batch."
  [analyzer measuring entries parse]
  (let [{:keys [git-executable root-directory]} analyzer
        unparsed-blobs (into []
                             (comp (remove #(cached-facts analyzer (:path %) (:blob %) measuring))
                                   (map :blob)
                                   (distinct))
                             entries)]
    (task/then (git/read-blobs git-executable root-directory unparsed-blobs)
               (fn [texts-by-blob]
                 (into []
                       (keep (fn [{:keys [path blob]}]
                               (let [cached (cached-facts analyzer path blob measuring)]
                                 (cond
                                   (= ::skipped cached) nil
                                   (some? cached) cached
                                   :else (when-let [text (get texts-by-blob blob)]
                                           (facts-for analyzer path blob text measuring parse))))))
                       entries)))))

(defn analyze-commit
  "Resolves to {:snapshot :notices} for the files of `commit`, a map from
  the history with :sha, :subject, and :time."
  [analyzer configuration {:keys [sha subject time]}]
  (if-let [remembered (remembered-snapshot analyzer sha)]
    (task/resolved {:snapshot remembered :notices []})
    (let [{:keys [git-executable root-directory]} analyzer
          wanted-file? (wanted-path-predicate configuration)]
      (task/then
       (git/tree-files git-executable root-directory sha)
       (fn [entries]
         (let [kept-entries (filterv #(wanted-file? (:path %)) entries)
               analyzed-entries (vec (take file-limit kept-entries))]
           (task/then
            (parser-for analyzer (map :path analyzed-entries))
            (fn [{:keys [parse error]}]
              (task/then (commit-facts analyzer (measuring-options configuration) analyzed-entries parse)
                         (fn [facts-list]
                           {:snapshot (remember-snapshot! analyzer
                                                          (snapshot/build-snapshot (wanted-facts configuration facts-list)
                                                                                   {:id sha
                                                                                    :label subject
                                                                                    :time time
                                                                                    :weights (:weights configuration)
                                                                                    :include-paths (:include-paths configuration)}))
                            :notices (filterv some? [(limit-notice (- (count kept-entries) (count analyzed-entries)))
                                                     (parser-notice error)])}))))))))))

(defn history
  "Resolves to recent commits that changed source files, newest first, or
  an empty vector outside a git repository."
  [analyzer configuration limit]
  (task/then (repository-root analyzer)
             (fn [repository-root-path]
               (if repository-root-path
                 (-> (git/commits (:git-executable analyzer) (:root-directory analyzer)
                                  {:limit limit :extensions (config/read-extensions configuration)})
                     (task/recover (fn [_error] [])))
                 []))))

(defn namespace-sources
  "Resolves to one page's files as [{:path :language :text :content-key}].
  Working tree text is read fresh, from `open-text` for files with unsaved
  changes, so its content key may be newer than the snapshot's."
  [analyzer configuration snapshot-value namespace-id open-text]
  (let [{:keys [git-executable root-directory]} analyzer
        measuring (measuring-options configuration)
        file-entries (get-in snapshot-value [:namespaces namespace-id :files])]
    (task/then
     (parser-for analyzer (map :path file-entries))
     (fn [{:keys [parse]}]
       (let [source-file (fn [path content-key text]
                           {:path path
                            :language (:language (facts-for analyzer path content-key text measuring parse) :plain)
                            :text (source/normalize-text text)
                            :content-key content-key})]
         (if (= working-tree (:id snapshot-value))
           (task/all
            (map (fn [{:keys [path]}]
                   (task/then (working-text analyzer (or open-text (constantly nil)) path)
                              (fn [text]
                                (let [text (or text "")]
                                  (source-file path (files/content-key text) text)))))
                 file-entries))
           (task/then (git/read-blobs git-executable root-directory (map :content-key file-entries))
                      (fn [texts-by-blob]
                        (mapv (fn [{:keys [path content-key]}]
                                (source-file path content-key (get texts-by-blob content-key "")))
                              file-entries)))))))))
