(ns codecloud.io.analyzer
  "Analyzes a cloud's root as its working tree or at any commit. Facts are
  cached by path and content, so only changed files parse again, and a
  working-tree file whose stamp has not changed is not read again."
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
   :working-facts (atom {})
   :history-facts (atom {:recent {} :older {}})
   :stamps (atom {})
   :working-build (atom nil)
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
  "Resolves to {:config :problems}, the configuration read from the
  defaults, then deps.edn's :codecloud/config, then the cloud file's
  text, each overriding the last."
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

(defn- measuring-options [configuration]
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
  "Facts remembered under `cache-key`, ::skipped for a file read before and
  left out, or nil for one never read."
  [analyzer cache-key]
  (let [{:keys [recent older]} @(:history-facts analyzer)]
    (or (get @(:working-facts analyzer) cache-key)
        (get recent cache-key)
        (get older cache-key))))

(defn- remember-history-facts!
  "Keeps facts read from history in the newer generation. Once that one
  fills it becomes the older, and the older is let go."
  [analyzer cache-key facts]
  (let [generation-limit 20000]
    (swap! (:history-facts analyzer)
           (fn [{:keys [recent older]}]
             (if (< (count recent) generation-limit)
               {:recent (assoc recent cache-key facts) :older older}
               {:recent {cache-key facts} :older recent})))))

(defn- unless-skipped [facts]
  (when-not (= ::skipped facts) facts))

(defn- facts-for
  "Facts for one file, nil when left out or unreadable, cached or read
  from `text`. `remember!` gets each cache entry used."
  [analyzer path content-key text measuring parse remember!]
  (let [cache-key (facts-key path content-key measuring)
        found (or (cached-facts analyzer cache-key)
                  (when text
                    (or (some-> (languages/analyze-file path text (assoc measuring :parse-tree parse))
                                (assoc :content-key content-key))
                        ::skipped)))]
    (when found
      (remember! cache-key found)
      (unless-skipped found))))

(defn- wanted-facts [configuration facts-list]
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
  "Drops remembered snapshots and file stamps, so the next reading builds
  afresh, for example after the configuration changed."
  [analyzer]
  (reset! (:snapshots analyzer) {:order [] :by-id {}})
  (reset! (:working-build analyzer) nil)
  (reset! (:stamps analyzer) {}))

(defn remembered-snapshot [analyzer sha]
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
  "Resolves to the text of the working tree file at `path`, which is what
  `open-text` holds for a file with unsaved changes, else what is on
  disk, or nil when unreadable."
  [analyzer open-text path]
  (if-let [text (open-text path)]
    (task/resolved text)
    (files/read-text (files/absolute-path (:root-directory analyzer) path))))

(defn- working-file-facts
  "Resolves to the facts of the working tree file at `path`. A file whose
  stamp is unchanged is not read again, unless its last reading came
  within seconds of a change, which a coarse clock could hide."
  [analyzer path measuring open-text parse remember!]
  (if-let [text (open-text path)]
    (task/resolved (facts-for analyzer path (files/content-key text) text measuring parse remember!))
    (let [file-path (files/absolute-path (:root-directory analyzer) path)
          stamped-at (js/Date.now)
          settle-milliseconds 3000]
      (task/then
       (files/stamp file-path)
       (fn [stamp]
         (let [known (get @(:stamps analyzer) path)
               known-key (when (and stamp (:settled? known) (= stamp (:stamp known)))
                           (facts-key path (:content-key known) measuring))
               known-facts (some->> known-key (cached-facts analyzer))]
           (cond
             (nil? stamp) nil
             known-facts (do (remember! known-key known-facts)
                             (unless-skipped known-facts))
             :else (task/then (files/read-text file-path)
                              (fn [text]
                                (when text
                                  (let [content-key (files/content-key text)]
                                    (swap! (:stamps analyzer) assoc path
                                           {:stamp stamp
                                            :content-key content-key
                                            :settled? (> (- stamped-at (first stamp)) settle-milliseconds)})
                                    (facts-for analyzer path content-key text measuring parse remember!))))))))))))

(defn- working-tree-facts
  "Resolves to the facts of each of `paths`, nil where unreadable. The
  cache keeps only the facts this reading used."
  [analyzer configuration paths on-progress open-text parse]
  (let [total (count paths)
        done-count (atom 0)
        progress-interval 40
        parallel-reads 16
        measuring (measuring-options configuration)
        used (atom {})
        remember! (fn [cache-key facts] (swap! used assoc cache-key facts))
        read-facts (fn [path]
                     (task/then (working-file-facts analyzer path measuring open-text parse remember!)
                                (fn [facts]
                                  (let [finished (swap! done-count inc)]
                                    (when (and on-progress (zero? (mod finished progress-interval)))
                                      (on-progress {:done finished :total total})))
                                  facts)))]
    (task/then (task/map-limited parallel-reads read-facts paths)
               (fn [facts-list]
                 (reset! (:working-facts analyzer) @used)
                 (swap! (:stamps analyzer) select-keys paths)
                 facts-list))))

(defn- working-snapshot
  "The working tree's snapshot of `facts`, the one built last when it read
  the same files with the same configuration."
  [analyzer configuration facts]
  (let [inputs [configuration (mapv (juxt :path :content-key) facts)]
        {built-inputs :inputs built :snapshot} @(:working-build analyzer)]
    (if (= inputs built-inputs)
      built
      (let [snapshot-value (snapshot/build-snapshot facts {:id working-tree
                                                           :label "Working tree"
                                                           :time (js/Date.now)
                                                           :weights (:weights configuration)
                                                           :include-paths (:include-paths configuration)})]
        (reset! (:working-build analyzer) {:inputs inputs :snapshot snapshot-value})
        snapshot-value))))

(defn analyze-working-tree
  "Resolves to {:snapshot :notices}, the last reading's very snapshot when
  nothing changed. `open-text` gives the text of files with unsaved
  changes."
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
                       {:snapshot (working-snapshot analyzer configuration (wanted-facts configuration facts-list))
                        :notices (filterv some? [(limit-notice (- (count paths) (count analyzed-paths)))
                                                 (parser-notice error)])}))))))))

(defn- commit-facts
  "Resolves to the facts of each of `entries`, {:path :blob}, from the
  caches, or parsed from blobs read out of git in one batch."
  [analyzer measuring entries parse]
  (let [{:keys [git-executable root-directory]} analyzer
        unparsed-blobs (into []
                             (comp (remove #(cached-facts analyzer (facts-key (:path %) (:blob %) measuring)))
                                   (map :blob)
                                   (distinct))
                             entries)
        remember! (fn [cache-key facts] (remember-history-facts! analyzer cache-key facts))]
    (task/then (git/read-blobs git-executable root-directory unparsed-blobs)
               (fn [texts-by-blob]
                 (into []
                       (keep (fn [{:keys [path blob]}]
                               (facts-for analyzer path blob (get texts-by-blob blob) measuring parse remember!)))
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
       (let [remember! (if (= working-tree (:id snapshot-value))
                         (fn [cache-key facts] (swap! (:working-facts analyzer) assoc cache-key facts))
                         (fn [cache-key facts] (remember-history-facts! analyzer cache-key facts)))
             source-file (fn [path content-key text]
                           {:path path
                            :language (:language (facts-for analyzer path content-key text measuring parse remember!) :plain)
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
