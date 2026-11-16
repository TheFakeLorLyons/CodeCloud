(ns codecloud.agents.cloud-tools.common
  "Helpers the cloud's tools share, for their wording, their arguments,
  paths within the cloud's folder, and finding pages and definitions."
  (:require [clojure.string :as string]
            [codecloud.counting :refer [counted]]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.source :as source]
            [codecloud.presentation.page :as page]))

;; The tools as agents see them

(def read-only
  "Annotations telling clients a tool only reads, so they may run it
  without asking first."
  {"readOnlyHint" true "openWorldHint" false})

(def marking
  "Annotations telling clients a tool changes only what the cloud shows
  the person."
  {"readOnlyHint" false "destructiveHint" false "idempotentHint" false "openWorldHint" false})

(def page-property
  {"type" "string"
   "description" "A page: a namespace such as app.core, or a file's path, absolute or relative to the workspace folder"})

;; Wording

(defn hundredths
  "`value` written with two decimal places, the same on every platform."
  [value]
  (let [scaled (Math/round (* 100.0 (double value)))]
    (str (quot scaled 100) "." (when (< (rem scaled 100) 10) "0") (rem scaled 100))))

(defn line-span [line end-line]
  (if (or (nil? end-line) (= line end-line))
    (str "line " line)
    (str "lines " line "-" end-line)))

(defn some-of
  "The first `most-listed` of `items` joined by commas, saying how many
  more there are."
  [items most-listed]
  (let [total (count items)]
    (str (string/join ", " (take most-listed items))
         (when (> total most-listed) (str ", and " (- total most-listed) " more")))))

(defn language-label [language-id]
  (:label (catalog/language language-id)))

(defn kind-word [node]
  (case (:kind node)
    :namespace "namespace"
    :script "script"
    "file"))

;; Arguments

(defn text-argument [arguments argument-key]
  (let [value (get arguments argument-key)]
    (if (string? value) (string/trim value) "")))

(defn count-argument [arguments argument-key default lowest highest]
  (let [value (get arguments argument-key)]
    (if (number? value) (min highest (max lowest (int value))) default)))

;; Paths

(defn forward-slashes [path-text]
  (string/replace path-text "\\" "/"))

(def absolute-start
  "The start of an absolute path with forward slashes, which is a slash or
  a Windows drive."
  #"^(?:/|[A-Za-z]:/)")

(defn normalized-path
  "`path-text` with . and .. steps followed, or nil when it climbs above
  where it starts."
  [path-text]
  (let [start (re-find absolute-start path-text)
        steps (reduce (fn [kept step]
                        (case step
                          ("" ".") kept
                          ".." (if (seq kept) (pop kept) (reduced nil))
                          (conj kept step)))
                      []
                      (string/split (subs path-text (count (or start ""))) #"/"))]
    (when steps (str start (string/join "/" steps)))))

(defn within-folder
  "`absolute-path` relative to the folder `folder`, or nil when it lies
  outside. A folder on a Windows drive compares without letter case."
  [folder absolute-path]
  (let [folder (string/replace folder #"/+$" "")
        fold (if (re-find #"^[A-Za-z]:/" folder) string/lower-case identity)]
    (cond
      (= (fold folder) (fold absolute-path)) ""
      (string/starts-with? (fold absolute-path) (fold (str folder "/"))) (subs absolute-path (inc (count folder)))
      :else nil)))

(defn cloud-paths
  "The paths relative to the cloud's folder that `given` may mean. An
  absolute path within the folder means one, and a relative path means
  itself as written and the same path within each workspace folder."
  [{:keys [root workspace-roots]} given]
  (let [root (forward-slashes (or root ""))
        given (forward-slashes given)]
    (if (re-find absolute-start given)
      (keep identity [(some->> (normalized-path given) (within-folder root))])
      (distinct (keep identity (cons (normalized-path given)
                                     (for [workspace-root workspace-roots]
                                       (some->> (normalized-path (str (forward-slashes workspace-root) "/" given))
                                                (within-folder root)))))))))

(defn file-index
  "Map from the path of every file of the snapshot to {:page-id
  :language}."
  [snapshot]
  (into {}
        (for [[page-id node] (:namespaces snapshot)
              {:keys [path language]} (:files node)]
          [path {:page-id page-id :language (or language (page/language-of node))}])))

;; Finding pages and definitions

(defn ids-ending-with
  "Ids of the pages whose id, or a path of whose files, ends with `given`
  after a dot or a slash, as orchard.core ends with core."
  [snapshot given]
  (let [wanted (string/lower-case (forward-slashes given))
        ends? (fn [candidate]
                (let [lowered (string/lower-case candidate)]
                  (or (string/ends-with? lowered (str "." wanted))
                      (string/ends-with? lowered (str "/" wanted)))))]
    (into (sorted-set)
          (keep (fn [[id node]] (when (or (ends? id) (some (comp ends? :path) (:files node))) id)))
          (:namespaces snapshot))))

(defn ids-holding
  "Ids of the pages whose id holds `given`, ignoring letter case."
  [snapshot given]
  (let [wanted (string/lower-case given)]
    (into (sorted-set) (filter #(string/includes? (string/lower-case %) wanted)) (keys (:namespaces snapshot)))))

(defn find-page
  "{:id} of the page `given` names by its id, by the path of one of its
  files, or by the end of either when only one page ends so; otherwise
  {:error} with the pages it might mean."
  [{:keys [snapshot] :as context} given]
  (let [most-listed 12]
    (cond
      (string/blank? given) {:error "Name a page: a namespace such as app.core, or a file's path."}
      (contains? (:namespaces snapshot) given) {:id given}
      :else
      (let [files (file-index snapshot)]
        (if-let [page-id (some #(get-in files [% :page-id]) (cloud-paths context given))]
          {:id page-id}
          (let [endings (ids-ending-with snapshot given)]
            (case (count endings)
              1 {:id (first endings)}
              0 (let [similar (ids-holding snapshot given)]
                  {:error (str "No page is named " given " or holds a file at that path."
                               (when (seq similar) (str " Pages whose names hold it: " (some-of similar most-listed) ".")))})
              {:error (str given " could mean " (some-of endings most-listed) ". Name one of them.")})))))))

(defn definition-named [node definition-name]
  (some #(when (= definition-name (:name %)) %) (:definitions node)))

(defn find-definition
  "{:id :definition} for the definition `arguments` name, its \"name\"
  qualified by its page, as app.db/save!, or bare beside a \"page\", or
  bare and defined by one page alone. Otherwise {:error}."
  [{:keys [snapshot] :as context} arguments]
  (let [given (text-argument arguments "name")
        page-given (text-argument arguments "page")
        nodes (:namespaces snapshot)
        most-listed 15]
    (cond
      (string/blank? given)
      {:error "Name a definition, such as app.db/save!."}

      (seq page-given)
      (let [{:keys [id error]} (find-page context page-given)
            bare (subs given (inc (or (string/last-index-of given "/") -1)))]
        (cond
          error {:error error}
          (definition-named (get nodes id) given) {:id id :definition (definition-named (get nodes id) given)}
          (definition-named (get nodes id) bare) {:id id :definition (definition-named (get nodes id) bare)}
          :else {:error (str id " defines nothing named " given ".")}))

      :else
      (let [slash-splits (for [index (reverse (keep-indexed (fn [index character] (when (= \/ character) index)) given))
                               :when (< 0 index (dec (count given)))]
                           [(subs given 0 index) (subs given (inc index))])
            qualified (some (fn [[page-part name-part]]
                              (let [{:keys [id]} (find-page context page-part)]
                                (when-let [definition (when id (definition-named (get nodes id) name-part))]
                                  {:id id :definition definition})))
                            slash-splits)
            definers (when-not qualified
                       (sort (keep (fn [[id node]] (when (definition-named node given) id)) nodes)))]
        (cond
          qualified qualified
          (= 1 (count definers)) {:id (first definers) :definition (definition-named (get nodes (first definers)) given)}
          (seq definers) {:error (str (count definers) " pages define " given ": "
                                      (some-of (map #(str % "/" given) definers) most-listed)
                                      ". Name it with its page.")}
          :else (let [wanted (string/lower-case given)
                      similar (sort (for [[id node] nodes
                                          {definition-name :name} (:definitions node)
                                          :when (string/includes? (string/lower-case definition-name) wanted)]
                                      (str id "/" definition-name)))]
                  {:error (str "No page defines " given "."
                               (when (seq similar) (str " Definitions whose names hold it: " (some-of similar most-listed) ".")))}))))))

(defn definition-around
  "The innermost definition of `node` in the file at `path` whose lines
  hold `line`, or nil."
  [node path line]
  (->> (:definitions node)
       (filter (fn [{definition-path :path start :line end :end-line}]
                 (and (= path definition-path) start (<= start line (or end start)))))
       (sort-by (fn [{start :line end :end-line}] (- (or end start) start)))
       first))

(defn page-label
  "A page's id, followed by its file's path when the id is a namespace's
  name."
  [node]
  (if (page/named-by-path? node)
    (:id node)
    (str (:id node) " (" (:path (page/primary-file node)) ")")))

(defn page-lines [node]
  (get-in node [:metrics :lines] 0))

(defn text-lines [text]
  (source/split-lines (source/normalize-text text)))

(defn clipped
  "`text` trimmed, and cut short past `longest` characters."
  [text longest]
  (let [trimmed (string/trim text)]
    (if (> (count trimmed) longest) (str (subs trimmed 0 longest) "…") trimmed)))

(defn folder-label [directory]
  (if (= "" directory) "." directory))

(defn cycle-chain [found-cycle]
  (string/join " → " (:path found-cycle)))

;; page_details

(defn edge-note
  "An edge's kind and its number of uses, as (load, 4 uses)."
  [{:keys [kind reference-count cross-language?]}]
  (str "(" (string/join ", " (remove nil? [(name (or kind :load))
                                           (when cross-language? "across languages")
                                           (when (pos? (or reference-count 0)) (counted reference-count "use"))]))
       ")"))

(defn edges-by-ends [snapshot]
  (into {} (map (juxt (juxt :from :to) identity)) (:edges snapshot)))

(defn cloud-relative
  "An absolute path relative to the cloud's folder when it lies there."
  [{:keys [root]} absolute-path]
  (when absolute-path
    (let [given (forward-slashes absolute-path)]
      (or (some-> root forward-slashes (within-folder given) not-empty) given))))
