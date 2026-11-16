(ns codecloud.vscode.agent-edits
  "Changes to files that no person made, handed to the agent hub to credit
  to an agent. They are edits to open documents that neither the person's
  typing, an undo or a redo, a save's formatting, nor a cloud made, and
  writes to disk no save in VS Code explains, as when an agent in a
  terminal edits a file. Each is found by comparing the file's lines with
  the text CodeCloud last knew of it."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.agents.presence :as presence]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.model.source :as source]
            [codecloud.vscode.agents :as agents]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.clouds :as clouds]))

(def ^:private most-texts
  "Files whose text CodeCloud keeps for finding what changed in them."
  600)

(defn create
  "The host's record of file changes, the text last known of each file,
  when documents began and finished saving, disk changes waiting to
  settle, and files being read."
  []
  (atom {:texts {} :order #queue [] :saving {} :saved {} :waiting {} :learning #{}}))

(defn- path-key
  "How the texts known are keyed: a path compared the way the file system
  compares it."
  [file-path]
  (clouds/comparison-path (path/normalize file-path)))

(defn- known-text [tracker file-path]
  (get-in @tracker [:texts (path-key file-path)]))

(defn- remember!
  "Keeps `text` as the text last known of the file at `file-path`; past
  `most-texts` files, the one known longest goes."
  [tracker file-path text]
  (let [text-key (path-key file-path)]
    (swap! tracker (fn [{:keys [texts order] :as current}]
                     (if (contains? texts text-key)
                       (assoc-in current [:texts text-key] text)
                       (let [grown-order (conj order text-key)
                             overflow (max 0 (- (count grown-order) most-texts))]
                         (assoc current
                                :texts (apply dissoc (assoc texts text-key text) (take overflow grown-order))
                                :order (nth (iterate pop grown-order) overflow))))))))

(defn- forget! [tracker file-path]
  (let [text-key (path-key file-path)]
    (swap! tracker (fn [current]
                     (-> current
                         (update :texts dissoc text-key)
                         (update :order #(into #queue [] (remove #{text-key}) %)))))))

(defn remember-texts!
  "Keeps the texts a cloud read, each [absolute-path text], of files whose
  text CodeCloud does not know yet. A file it knows keeps the text its
  changes left, so a change still on its way is not lost."
  [{:keys [agent-edits]} path-texts]
  (doseq [[file-path text] path-texts
          :when (and (string? text) (nil? (known-text agent-edits file-path)))]
    (remember! agent-edits file-path text)))

(defn- learn-touched-files!
  "Reads the text of each file in `presence-summary` that an agent read or
  works on and CodeCloud knows no text of, so a change an agent makes to
  one next, often with tools of its own, is found by its lines."
  [{:keys [agent-edits]} presence-summary]
  (doseq [file-path (into #{}
                          (comp (mapcat (fn [{:keys [place reads]}] (cons (:path place) (map :path reads))))
                                (remove nil?))
                          (:agents presence-summary))
          :let [text-key (path-key file-path)]
          :when (and (nil? (known-text agent-edits file-path))
                     (not (contains? (:learning @agent-edits) text-key)))]
    (swap! agent-edits update :learning conj text-key)
    (task/then (files/read-text file-path)
               (fn [text]
                 (swap! agent-edits update :learning disj text-key)
                 (when (and text (nil? (known-text agent-edits file-path)))
                   (remember! agent-edits file-path (source/normalize-text text)))))))

(defn- text-lines [text]
  (string/split text #"\n" -1))

(defn- change-between
  "The change from `before` to `after` of `file-path` as
  codecloud.agents.hub/note-change! takes it, its lines when `before` is
  known, else the whole file, or nil."
  [file-path before after]
  (cond
    (nil? before) {:path file-path}
    (= before after) nil
    :else (some-> (presence/line-change (text-lines before) (text-lines after)) (assoc :path file-path))))

(defn- typed?
  "True when an edit to `document` looks typed by the person, in the
  focused window's active editor, unsaved, with a caret within two lines
  of it."
  [document spans]
  (let [reach 2]
    (boolean
     (and (api/window-focused?)
          (api/document-dirty? document)
          (when-let [caret-lines (api/active-caret-lines document)]
            (some (fn [[first-line last-line]]
                    (some #(<= (- first-line reach) (inc %) (+ last-line reach)) caret-lines))
                  spans))))))

(defn- saving?
  "True while VS Code saves the file at `text-key`, when formatters and
  other save actions edit it on the person's behalf."
  [tracker text-key now]
  (let [;; VS Code gives save actions a few seconds before it writes.
        save-allowance-milliseconds 5000]
    (when-let [began (get-in @tracker [:saving text-key])]
      (< (- now began) save-allowance-milliseconds))))

(defn- document-edited!
  "Follows an edit to an open document. One that no person and no cloud
  made goes to the hub while an agent is connected, and the document's
  text becomes the text known."
  [{:keys [agent-edits] :as host} cloud-editing? document {:keys [reason spans]}]
  (let [file-path (api/document-path document)
        before (known-text agent-edits file-path)
        after (source/normalize-text (api/document-text document))]
    (remember! agent-edits file-path after)
    (when (and (agents/agents-present? host)
               (nil? reason)
               (not (cloud-editing? file-path))
               (not (saving? agent-edits (path-key file-path) (js/Date.now)))
               (not (typed? document spans)))
      (when-let [change (change-between file-path before after)]
        (agents/note-change! host change)))))

(defn- open-document-at [file-path]
  (let [wanted (path-key file-path)]
    (some #(when (= wanted (path-key (api/document-path %))) %) (api/open-documents))))

(defn- follow-disk-change!
  "Follows a settled change on disk to `file-path`. A save or reload
  explains it, a write under unsaved changes covers the whole file, and
  any other goes to the hub while an agent is connected."
  [{:keys [agent-edits] :as host} file-path]
  (let [text-key (path-key file-path)
        recent-save-milliseconds 3000
        saved-at (get-in @agent-edits [:saved text-key])
        document (open-document-at file-path)
        agent-present? (agents/agents-present? host)]
    (cond
      (and saved-at (< (- (js/Date.now) saved-at) recent-save-milliseconds)) (task/resolved nil)
      (and document (not (api/document-dirty? document))) (task/resolved nil)
      document (task/resolved (when agent-present? (agents/note-change! host {:path file-path})))
      (not (or agent-present? (known-text agent-edits file-path))) (task/resolved nil)
      :else (task/then (files/read-text file-path)
                       (fn [text]
                         (let [before (known-text agent-edits file-path)
                               after (some-> text source/normalize-text)
                               change (cond
                                        (not agent-present?) nil
                                        after (change-between file-path before after)
                                        before (some-> (presence/line-change (text-lines before) []) (assoc :path file-path)))]
                           (if after (remember! agent-edits file-path after) (forget! agent-edits file-path))
                           (when change (agents/note-change! host change))))))))

(defn disk-changed!
  "Notes that the file at `file-path`, which a cloud shows, changed on
  disk. Writes come in bursts, a file emptied before it is filled, so the
  change counts once the file is quiet for a moment."
  [{:keys [agent-edits] :as host} file-path]
  (let [text-key (path-key file-path)
        settle-milliseconds 200]
    (js/clearTimeout (get-in @agent-edits [:waiting text-key]))
    (swap! agent-edits assoc-in [:waiting text-key]
           (js/setTimeout (fn []
                            (swap! agent-edits update :waiting dissoc text-key)
                            (follow-disk-change! host file-path))
                          settle-milliseconds))))

(defn activate!
  "Learns the text of open documents and of files agents touch, and
  follows documents from now on, returning the disposables.
  `cloud-editing?` tells whether a cloud is applying an edit to a path."
  [{:keys [agent-edits presence-summary] :as host} cloud-editing?]
  (let [learn! (fn [document]
                 (remember! agent-edits (api/document-path document) (source/normalize-text (api/document-text document))))]
    (doseq [document (api/open-documents)]
      (learn! document))
    (add-watch presence-summary ::learning (fn [_key _reference previous current]
                                             (when (not= previous current)
                                               (learn-touched-files! host current))))
    [(api/disposable #(remove-watch presence-summary ::learning))
     (api/on-document-open! learn!)
     (api/on-document-close! (fn [document]
                               ;; Closing without saving leaves the file as it was on disk.
                               (when (api/document-dirty? document)
                                 (forget! agent-edits (api/document-path document)))))
     (api/on-document-edit! (fn [document edit] (document-edited! host cloud-editing? document edit)))
     (api/on-document-will-save! (fn [document]
                                   (swap! agent-edits assoc-in [:saving (path-key (api/document-path document))] (js/Date.now))))
     (api/on-document-save! (fn [document]
                              (let [text-key (path-key (api/document-path document))]
                                (swap! agent-edits #(-> %
                                                        (update :saving dissoc text-key)
                                                        (assoc-in [:saved text-key] (js/Date.now))))
                                (learn! document))))]))
