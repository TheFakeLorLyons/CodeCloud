(ns codecloud.presentation.visibility
  "Which pages the filter shows: named views, folders and files shown
  alone or hidden, and hidden languages. The view measures the shown pages
  among themselves, so hiding pages moves the weights, colors, and layout
  of the rest."
  (:require [clojure.string :as string]
            [codecloud.collections :as collections]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.selection :as selection]
            [codecloud.model.source :as source]
            [codecloud.presentation.page :as page]))

(def no-filter
  {:active-views #{} :only [] :hidden [] :hidden-languages #{}})

(defn filtering?
  "True when the filter hides anything."
  [{:keys [active-views only hidden hidden-languages]}]
  (boolean (or (seq active-views) (seq only) (seq hidden) (seq hidden-languages))))

(defn- active-selections [named-views active-views]
  (into [] (keep #(get named-views %)) (sort active-views)))

(defn- choices
  "Compiled selections a page must fall in, one of them at least, or an
  empty vector when everything may show."
  [named-views {:keys [active-views only]}]
  (mapv selection/compile-selection
        (cond-> (active-selections named-views active-views)
          (seq only) (conj {:include only}))))

(defn visible-ids
  "Ids of the pages in `snapshot` that the filter shows, given the cloud's
  named views. A page shows when any of its files does and its language
  is not hidden."
  [snapshot named-views {:keys [hidden hidden-languages] :as filter-state}]
  (let [compiled-choices (choices named-views filter-state)
        hidden-matcher (when (seq hidden) (selection/compile-selection {:include hidden}))
        hidden-language? (set hidden-languages)
        shown-path? (fn [path]
                      (and (or (empty? compiled-choices)
                               (some #(selection/selected? % path) compiled-choices))
                           (not (and hidden-matcher (selection/selected? hidden-matcher path)))))]
    (into #{}
          (comp (filter (fn [[_id node]]
                          (and (not (hidden-language? (page/language-of node)))
                               (some (comp shown-path? :path) (:files node)))))
                (map key))
          (:namespaces snapshot))))

(defn show-only
  "A filter showing only what `patterns` match."
  [patterns]
  (assoc no-filter :only (vec (distinct patterns))))

(defn hide
  "The filter with `patterns` hidden as well."
  [filter-state patterns]
  (update filter-state :hidden #(vec (distinct (concat % patterns)))))

(defn toggle-view
  "The filter with the named view switched on or off."
  [filter-state view-label]
  (update filter-state :active-views
          #(let [active (set %)]
             (if (contains? active view-label) (disj active view-label) (conj active view-label)))))

(defn namespace-patterns
  "Patterns matching the files of one page."
  [snapshot namespace-id]
  (mapv (comp selection/file-pattern :path) (get-in snapshot [:namespaces namespace-id :files])))

(defn neighborhood-patterns
  "Patterns matching a page and every page it requires or that requires
  it."
  [snapshot namespace-id]
  (let [node (get-in snapshot [:namespaces namespace-id])]
    (vec (distinct (mapcat #(namespace-patterns snapshot %)
                           (cons namespace-id (concat (:requires node) (:required-by node))))))))

;; Showing and hiding pages, folders, and languages

(defn- inside?
  "True when `path` lies in the folder `directory`, at any depth."
  [path directory]
  (or (= "" directory) (string/starts-with? path (str directory "/"))))

(defn- pattern-path
  "The folder or file an anchored pattern names, as {:path :folder?}, or
  nil for a pattern of any other shape."
  [pattern]
  (cond
    (= "/**" pattern) {:path "" :folder? true}
    (or (not (string/starts-with? pattern "/")) (re-find #"[*?\[]" pattern)) nil
    (string/ends-with? pattern "/") {:path (subs pattern 1 (dec (count pattern))) :folder? true}
    :else {:path (subs pattern 1) :folder? false}))

(defn- page-paths
  "Paths of the files of the pages `ids`."
  [snapshot ids]
  (into [] (comp (mapcat #(get-in snapshot [:namespaces % :files])) (map :path)) ids))

(defn pages-under
  "Ids of the pages whose folder is `directory` or lies inside it."
  [snapshot directory]
  (into #{}
        (comp (filter (fn [[_id node]]
                        (let [folder (page/directory node)]
                          (or (= folder directory) (inside? folder directory)))))
              (map key))
        (:namespaces snapshot)))

(defn pages-in-language
  "Ids of the pages in `language-id`."
  [snapshot language-id]
  (into #{}
        (comp (filter (fn [[_id node]] (= language-id (page/language-of node))))
              (map key))
        (:namespaces snapshot)))

(defn hide-pages
  "The filter with the pages `ids` hidden as well."
  [filter-state snapshot ids]
  (hide filter-state (mapv selection/file-pattern (page-paths snapshot ids))))

(defn hide-folder
  "The filter with everything under `directory` hidden, dropping the hidden
  patterns inside it that it now covers."
  [filter-state directory]
  (-> filter-state
      (update :hidden (fn [hidden]
                        (vec (remove (fn [pattern]
                                       (when-let [{:keys [path]} (pattern-path pattern)]
                                         (and (not= "" path) (inside? path directory))))
                                     hidden))))
      (hide [(selection/directory-pattern directory)])))

(defn hide-language
  "The filter with the pages of `language-id` hidden as well."
  [filter-state language-id]
  (update filter-state :hidden-languages #(conj (set %) language-id)))

(defn- split-folder
  "Patterns hiding everything under `directory` except the files in
  `kept-paths`: whole subfolders that hold none of them, and single files
  beside them."
  [directory kept-paths paths]
  (let [prefix (if (= "" directory) "" (str directory "/"))
        children (group-by (fn [path]
                             (let [rest-of-path (subs path (count prefix))
                                   slash-index (string/index-of rest-of-path "/")]
                               (if slash-index
                                 [:folder (str prefix (subs rest-of-path 0 slash-index))]
                                 [:file path])))
                           (filter #(inside? % directory) paths))]
    (into []
          (mapcat (fn [[[kind child-path] child-paths]]
                    (cond
                      (= :file kind) (when-not (contains? kept-paths child-path)
                                       [(selection/file-pattern child-path)])
                      (some kept-paths child-paths) (split-folder child-path kept-paths paths)
                      :else [(selection/directory-pattern child-path)])))
          (sort-by key children))))

(defn- spared-patterns
  "What hidden `pattern` becomes once the files in `kept-paths` must show:
  the pattern itself when it hides none of them, and otherwise patterns
  for the rest of what it hid."
  [pattern kept-paths paths]
  (let [matcher (selection/compile-selection {:include [pattern]})
        matched (filterv #(selection/selected? matcher %) paths)
        {:keys [path folder?]} (pattern-path pattern)]
    (cond
      (not-any? kept-paths matched) [pattern]
      folder? (split-folder path kept-paths paths)
      :else (into [] (comp (remove kept-paths) (map selection/file-pattern)) matched))))

(defn reveal
  "The filter changed as little as it can so the pages `ids` show: a hidden
  language gives way, with its other pages hidden one by one; a hidden
  pattern over them splits into the parts around them; and pages outside
  the active views join :only."
  [filter-state snapshot named-views ids]
  (let [target-ids (set ids)
        kept-paths (set (page-paths snapshot target-ids))
        paths (page-paths snapshot (keys (:namespaces snapshot)))
        hidden-language? (set (:hidden-languages filter-state))
        revealed-languages (into #{}
                                 (comp (map #(page/language-of (get-in snapshot [:namespaces %])))
                                       (filter hidden-language?))
                                 target-ids)
        language-siblings (into []
                                (comp (mapcat #(pages-in-language snapshot %))
                                      (remove target-ids))
                                (sort revealed-languages))
        spared (-> filter-state
                   (update :hidden-languages #(reduce disj (set %) revealed-languages))
                   (hide-pages snapshot (sort language-siblings))
                   (update :hidden (fn [hidden]
                                     (vec (distinct (mapcat #(spared-patterns % kept-paths paths) hidden))))))
        compiled-choices (choices named-views spared)
        outside (when (seq compiled-choices)
                  (remove (fn [id]
                            (some (fn [path] (some #(selection/selected? % path) compiled-choices))
                                  (page-paths snapshot [id])))
                          (sort target-ids)))]
    (cond-> spared
      (seq outside) (update :only #(vec (distinct (concat % (map selection/file-pattern
                                                                (page-paths snapshot outside)))))))))

(defn shown-share
  "How much of `ids` shows among `visible-ids`: :all, :none, or :some."
  [visible-ids ids]
  (let [shown-count (count (filter visible-ids ids))]
    (cond
      (zero? shown-count) :none
      (= shown-count (count ids)) :all
      :else :some)))

(defn toggle-pages
  "Hides the pages `ids` when all of them show, and reveals them all
  otherwise, as a checkbox goes from mixed to checked."
  [filter-state snapshot named-views visible-ids ids]
  (if (every? visible-ids ids)
    (hide-pages filter-state snapshot (sort ids))
    (reveal filter-state snapshot named-views ids)))

(defn toggle-folder
  "Hides the folder when every page under it shows, and reveals them all
  otherwise."
  [filter-state snapshot named-views visible-ids directory]
  (let [ids (pages-under snapshot directory)]
    (if (and (seq ids) (every? visible-ids ids))
      (hide-folder filter-state directory)
      (reveal filter-state snapshot named-views ids))))

(defn toggle-language
  "Hides a language when every page of it shows, and reveals them all
  otherwise."
  [filter-state snapshot named-views visible-ids language-id]
  (let [ids (pages-in-language snapshot language-id)]
    (if (and (seq ids) (every? visible-ids ids))
      (hide-language filter-state language-id)
      (reveal filter-state snapshot named-views ids))))

(defn folder-tree
  "The folders holding the snapshot's pages as nested {:path :page-ids
  :all-ids :folders}, :page-ids being the pages directly inside and
  :all-ids every page at any depth."
  [snapshot]
  (let [by-directory (group-by #(page/directory (val %)) (:namespaces snapshot))
        with-ancestors (fn [directory]
                         (let [segments (string/split directory #"/")]
                           (map #(string/join "/" (take % segments)) (range 1 (inc (count segments))))))
        directories (into #{""} (mapcat with-ancestors) (remove #{""} (keys by-directory)))
        children-of (group-by source/directory-of (disj directories ""))
        build (fn build [directory]
                (let [folders (mapv build (sort (get children-of directory)))
                      page-ids (vec (sort (map key (get by-directory directory))))]
                  {:path directory
                   :page-ids page-ids
                   :all-ids (into (set page-ids) (mapcat :all-ids) folders)
                   :folders folders}))]
    (build "")))

;; Views

(defn as-selection
  "The filter as one selection, for saving it as a named view. Hidden
  languages become the files of their pages."
  [named-views {:keys [active-views only hidden hidden-languages]} snapshot]
  (let [selections (active-selections named-views active-views)
        everything? (some #(empty? (:include %)) selections)
        language-paths (page-paths snapshot (sort (mapcat #(pages-in-language snapshot %) hidden-languages)))]
    {:include (if everything? [] (vec (distinct (concat (mapcat :include selections) only))))
     :ignore (vec (distinct (concat (mapcat :ignore selections) hidden (map selection/file-pattern language-paths))))}))

(defn pattern-label
  "A pattern as a reader would name it: src/app for /src/app/."
  [pattern]
  (if (= "/**" pattern)
    "everything"
    (-> pattern
        (string/replace #"^/" "")
        (string/replace #"/$" ""))))

(defn- shortened [labels most]
  (if (> (count labels) most)
    (conj (vec (take most labels)) (str (- (count labels) most) " more"))
    labels))

(defn describe
  "What the filter shows and hides, or nil."
  [{:keys [active-views only hidden hidden-languages] :as filter-state}]
  (when (filtering? filter-state)
    (let [most-named 3
          view-phrase (when (seq active-views)
                        (str "the " (collections/spoken-list (sort active-views))
                             (if (= 1 (count active-views)) " view" " views")))
          shown (cond-> []
                  view-phrase (conj view-phrase)
                  (seq only) (into (shortened (map pattern-label only) most-named)))
          hidden-labels (shortened (concat (sort (map (comp :label catalog/language) hidden-languages))
                                           (map pattern-label hidden))
                                   most-named)]
      (string/join " " (cond-> []
                         (seq shown) (conj (str "Showing " (collections/spoken-list shown) "."))
                         (seq hidden-labels) (conj (str "Hiding " (collections/spoken-list hidden-labels) ".")))))))
