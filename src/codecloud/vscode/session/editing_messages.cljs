(ns codecloud.vscode.session.editing-messages
  "A session's handlers for editing from the cloud, which are edits,
  formatting, new files, and saving, and its following of documents edited
  elsewhere."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.new-file :as new-file]
            [codecloud.model.source :as source]
            [codecloud.presentation.clojure-format :as clojure-format]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.session.common :as common]
            [codecloud.vscode.session.snapshots :as snapshots]))

(defn- document-message
  "A :host/document message carrying `document`'s text for `path`, which
  edits from the view build on from now, as their new :epoch."
  [{:keys [state]} path document]
  (let [epoch (get-in (swap! state update-in [:edits path :epoch] (fnil inc 0)) [:edits path :epoch])
        text (api/document-text document)]
    (protocol/message :host/document {:path path
                                      :epoch epoch
                                      :text (source/normalize-text text)
                                      :content-key (files/content-key text)
                                      :dirty? (api/document-dirty? document)})))

(defn- apply-edit!
  "Applies one edit from the view to its document unless it builds on
  text the view has since replaced, or the document no longer holds the
  text it replaces, in which case the view gets the document's text."
  [{:keys [root-directory state] :as session} {edit-range :range :keys [path epoch revision text replaced]}]
  (if (not= epoch (get-in @state [:edits path :epoch] 0))
    (task/resolved nil)
    (task/then
     (api/open-document (path/join root-directory path))
     (fn [document]
       (if (not= replaced (source/normalize-text (api/text-in-range document edit-range)))
         (common/post! session (document-message session path document))
         (let [;; The cloud reads the code again after twice as long as the
               ;; last reading took, kept within these bounds, so typing
               ;; redraws dependencies at its pauses without a large
               ;; codebase's readings crowding the editor.
               shortest-settle 700
               longest-settle 10000
               edit-settle-milliseconds (min longest-settle (max shortest-settle (* 2 (:analysis-milliseconds @state 0))))]
           (swap! state update-in [:edits path :applying] (fnil inc 0))
           (-> (api/replace-range! document edit-range (cond-> text
                                                    (api/crlf-document? document) (string/replace "\n" "\r\n")))
               (task/recover (fn [_error] false))
               (task/then (fn [applied?]
                            (swap! state update-in [:edits path :applying] dec)
                            (if applied?
                              (do (common/post! session (protocol/message :host/edited
                                                                   {:path path
                                                                    :revision revision
                                                                    :content-key (files/content-key (api/document-text document))
                                                                    :dirty? (api/document-dirty? document)}))
                                  (common/debounce! session :analyze edit-settle-milliseconds #(snapshots/analyze! session)))
                              (common/post! session (document-message session path document))))))))))))

(defmethod common/handle-message :view/edit [session message]
  (common/in-turn! session #(apply-edit! session message)))

(defn- clojure-formatter?
  "True when Calva, whose formatter follows the project's cljfmt
  settings, formats Clojure in this VS Code."
  []
  (api/extension-active? "betterthantomorrow.calva"))

(defmethod common/handle-message :view/format [{:keys [root-directory] :as session} {:keys [path language revision epoch text start-line end-line]}]
  (let [lines (editing-state/text-lines text)
        clojure? (= :clojure language)
        reindented (fn []
                     (let [[first-line last-line] (if start-line [start-line end-line] [0 (dec (count lines))])]
                       (clojure-format/reindent lines first-line first-line last-line)))
        post-formatted! (fn [formatted-text]
                          (common/post! session (protocol/message :host/formatted {:path path :revision revision :epoch epoch
                                                                            :text formatted-text})))]
    ;; In turn, so the document holds every edit sent before; a document
    ;; holding other text leaves the formatter out.
    (common/in-turn! session
                     (fn []
                (-> (api/open-document (path/join root-directory path))
                    (task/then (fn [document]
                                 (let [document-text (source/normalize-text (api/document-text document))]
                                   (if (= lines (editing-state/text-lines document-text))
                                     (task/then (api/format-edits document start-line end-line)
                                                (fn [edits] {:document-lines (string/split document-text #"\n" -1) :edits edits}))
                                     {:edits nil}))))
                    (task/recover (fn [error]
                                    (common/log! session (str "Formatting " path " failed: " (task/error-text error)))
                                    {:edits nil}))
                    (task/then (fn [{:keys [document-lines edits]}]
                                 (post-formatted!
                                  (cond
                                    (seq edits) (string/join "\n" (editing-state/with-edits document-lines edits))
                                    (and (some? edits) (or (not clojure?) (clojure-formatter?))) text
                                    clojure? (string/join "\n" (reindented))
                                    :else text)))))))))

(defmethod common/handle-message :view/new-file [{:keys [root-directory state panel] :as session} {:keys [directory]}]
  (let [prefix (if (string/blank? directory) "" (str (string/replace directory #"/+$" "") "/"))]
    (-> (api/input-box {:title "New File"
                        :prompt "The file's path within the cloud's folder. Folders it names that do not exist yet are made."
                        :placeholder "src/app/new_file.clj"
                        :value prefix
                        :selection [(count prefix) (count prefix)]
                        :validate (fn [typed]
                                    (or (new-file/name-problem typed)
                                        (when (files/path-exists? (files/absolute-path root-directory (string/trim typed)))
                                          "A file of that name is there already.")))})
        (task/then (fn [typed]
                     (when typed
                       (let [relative (string/replace (path/normalize (string/trim typed)) "\\" "/")
                             [text line column] (new-file/starting-text (:working-snapshot @state) relative)
                             absolute (files/absolute-path root-directory relative)]
                         (task/then (files/write-text absolute text)
                                    (fn [_]
                                      (common/log! session (str "Made " relative))
                                      (if (and (:analyzed? (catalog/language-by-name relative))
                                               ((analyzer/wanted-path-predicate (:configuration @state)) relative))
                                        (do (common/post! session (protocol/message :host/created {:path relative :line line :column column}))
                                            (snapshots/analyze! session))
                                        ;; A file the cloud leaves out, or shows read-only,
                                        ;; opens in an editor instead.
                                        (api/show-text-document! absolute (inc line) (inc column) (api/panel-view-column panel)))))))))
        (task/recover (fn [error] (common/post-notice! session (str "CodeCloud could not make the file: " (task/error-text error))))))))

(defmethod common/handle-message :view/save [{:keys [root-directory] :as session} {:keys [path]}]
  (common/in-turn! session
                   (fn []
              (-> (api/open-document (path/join root-directory path))
                  (task/then api/save-document!)
                  (task/recover (fn [error] (common/post-notice! session (str "Saving " path " failed: " (task/error-text error)))))))))

(defn applying-edit?
  "True while this session applies an edit from its view to the file at
  `file-path`."
  [{:keys [root-directory state]} file-path]
  (let [relative (string/replace (path/relative root-directory file-path) "\\" "/")]
    (pos? (get-in @state [:edits relative :applying] 0))))

(defn- edited-path [{:keys [root-directory state]} document]
  (let [relative (string/replace (path/relative root-directory (api/document-path document)) "\\" "/")]
    (when (contains? (:edits @state) relative) relative)))

(defn follow-edited-document!
  "Keeps the view in step with a document it edits, sending the new text
  after a change from elsewhere, and whether it is unsaved after a save
  or a return to the saved text. The view's own edits report themselves."
  [{:keys [state] :as session} document text-changed?]
  (when-let [path (edited-path session document)]
    (cond
      (pos? (get-in @state [:edits path :applying] 0)) nil
      text-changed? (common/post! session (document-message session path document))
      :else (common/post! session (protocol/message :host/dirty {:path path :dirty? (api/document-dirty? document)})))))
