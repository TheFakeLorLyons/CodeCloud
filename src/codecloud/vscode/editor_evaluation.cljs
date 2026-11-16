(ns codecloud.vscode.editor-evaluation
  "Evaluation from VS Code's text editors through a cloud's REPL, with
  the scopes the cloud's own pages have. The cloud's view sends the code
  to its REPL tab that evaluates, which echoes it, and each result shows
  after its code in the editor until the file changes."
  (:require [clojure.string :as string]
            [codecloud.model.source :as source]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.repl-state :as repl-state]
            [codecloud.vscode.api :as api]))

(defn create
  "The editor evaluations of a host, before any."
  []
  (atom {:requests-made 0 :pending {} :results {} :decorations nil}))

(def ^:private result-colors
  "The theme color each kind of result is written in."
  {:pending "editorCodeLens.foreground"
   :value "editorCodeLens.foreground"
   :error "editorError.foreground"})

(defn- decorations! [results]
  (or (:decorations @results)
      (let [made (update-vals result-colors api/line-end-decoration-type)]
        (swap! results assoc :decorations made)
        made)))

(defn- shown-text
  "A result as it reads after its code, a value after => or an error as it
  is, each on one line and cut short when long."
  [{:keys [status text]}]
  (let [longest-shown 120
        one-line (string/replace (str text) #"\s*\n\s*" " ")
        clipped (if (> (count one-line) longest-shown) (str (subs one-line 0 longest-shown) "…") one-line)]
    (case status
      :pending "…"
      :value (str "=> " clipped)
      clipped)))

(defn- tell-keybindings!
  "Tells VS Code's keybindings whether the active editor shows a result,
  so Escape takes results away there and nowhere else."
  [results]
  (let [active-path (:path (api/active-editor-view))]
    (api/set-context! "codecloud.hasInlineResults"
                      (boolean (and active-path (seq (get-in @results [:results active-path])))))))

(defn- show-file! [results file-path]
  (let [by-line (get-in @results [:results file-path])]
    (doseq [[kind decoration-type] (decorations! results)]
      (api/show-line-ends! file-path decoration-type
                           (for [[line result] by-line
                                 :when (= kind (:status result))]
                             {:line line :text (shown-text result)})))
    (tell-keybindings! results)))

(defn clear!
  "Takes away every result shown in the editors. Replies still coming
  show nothing."
  [results]
  (let [file-paths (keys (:results @results))]
    (swap! results assoc :results {})
    (doseq [file-path file-paths]
      (show-file! results file-path))))

(defn request
  "What evaluating `scope` in the active editor sends, with the :scope,
  the file's :path, and the zero-based :result-line where the result
  shows, or {:notice} saying why nothing can."
  [scope]
  (if-let [{:keys [path text language-id line column anchor-line anchor-column]} (api/active-editor-selection)]
    (if (not= "clojure" language-id)
      {:notice "Only Clojure files evaluate in the REPL."}
      (let [lines (editing-state/text-lines (source/normalize-text text))
            found (repl-state/evaluation {:lines lines :caret [line column] :anchor [anchor-line anchor-column] :language :clojure
                                          :path path}
                                         scope)
            [start-line] (:start found)
            [end-line end-column] (:end found)]
        (if (:notice found)
          found
          (assoc found
                 :scope scope
                 :path path
                 ;; A selection of whole lines ends at the start of the next.
                 :result-line (if (and (zero? end-column) (> end-line start-line)) (dec end-line) end-line)))))
    {:notice "Put the caret in a Clojure file in an editor to evaluate from it."}))

(defn remember!
  "Gives `request` an id and marks the line where its result will show.
  Returns the request with its :id."
  [results {:keys [path result-line] :as request}]
  (let [number (:requests-made (swap! results update :requests-made inc))
        id (str "editor-" number)]
    (swap! results #(-> %
                        (assoc-in [:pending id] {:path path :line result-line})
                        (assoc-in [:results path result-line] {:id id :status :pending})))
    (show-file! results path)
    (assoc request :id id)))

(defn note-reply!
  "Follows a reply the REPL sends the cloud's view: a reply to an
  editor's request updates the result shown after its code."
  [results {message-type :type :keys [id status] :as reply}]
  (when (= :host/repl-output message-type)
    (when-let [{:keys [path line]} (get-in @results [:pending id])]
      (let [words (set status)]
        (swap! results (fn [current]
                         (cond-> current
                           (= id (get-in current [:results path line :id]))
                           (update-in [:results path line] repl-state/answered-result reply words)
                           (contains? words "done") (update :pending dissoc id))))
        (show-file! results path)))))

(defn activate!
  "Keeps results in step with the editors, dropping a file's results when
  its text changes, showing them as an editor comes into view, and
  following the active editor for Escape."
  [results]
  [(api/on-document-change! (fn [document text-changed?]
                              (let [file-path (api/document-path document)]
                                (when (and text-changed? (seq (get-in @results [:results file-path])))
                                  (swap! results update :results dissoc file-path)
                                  (show-file! results file-path)))))
   (api/on-visible-editors-change! (fn []
                                     (doseq [file-path (keys (:results @results))]
                                       (show-file! results file-path))))
   (api/on-active-editor-change! #(tell-keybindings! results))
   (api/disposable (fn []
                     (api/dispose! (vals (:decorations @results)))))])
