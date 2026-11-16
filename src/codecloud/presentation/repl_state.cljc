(ns codecloud.presentation.repl-state
  "The REPL pane's state. The pane holds tabs, each a REPL joined to an
  nREPL server or to Calva's REPL, or a shell in one of VS Code's
  terminals, with a log, a history, and requests of its own, and
  evaluation from the page being edited, the namespace followed from the
  page last clicked, and its place and size. Functions take the whole
  view state and return {:state :effects}, as the handlers of
  codecloud.presentation.ui-state do."
  (:require [clojure.string :as string]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.forms-state :as forms-state]
            [codecloud.presentation.handling :refer [changed send-effect unchanged with-notice]]
            [codecloud.presentation.page :as page]))

(def initial
  {:open? false
   :tabs []
   :active nil
   :evaluating nil
   :tabs-made 0
   :requests-made 0
   :sessions {}
   :following nil
   :geometry nil
   :menu-open? false
   :terminals []
   :calva? false})

(defn- session-path [tab] [:repl :sessions tab])

(defn session [state tab]
  (get-in state (session-path tab)))

(defn active-session
  "The session of the tab the pane shows, or nil when there is none."
  [state]
  (session state (get-in state [:repl :active])))

;; The log

(defn- with-log
  "The state with `text` of `kind` added to tab `tab`'s log for request
  `id`, joining the last entry when it continues the same output. The log
  keeps its latest entries, each cut to its latest text."
  [state tab kind text id]
  (let [kept-entries 400
        longest-entry 20000
        clipped (fn [entry-text]
                  (if (> (count entry-text) longest-entry)
                    (subs entry-text (- (count entry-text) longest-entry))
                    entry-text))
        {:keys [log entries-made]} (session state tab)
        last-entry (peek log)]
    (cond
      (nil? (session state tab)) state

      (and last-entry (contains? #{:out :err} kind) (= kind (:kind last-entry)) (= id (:id last-entry)))
      (assoc-in state (conj (session-path tab) :log (dec (count log))) (update last-entry :text #(clipped (str % text))))

      :else
      (let [kept (if (>= (count log) kept-entries) (subvec log (- (count log) (dec kept-entries))) log)]
        (update-in state (session-path tab) assoc
                   :log (conj kept {:number entries-made :kind kind :text (clipped text) :id id})
                   :entries-made (inc entries-made))))))

(defn- echoed
  "How code sent from a file shows in the log, its lines, as many as a
  short form has, marked when more follow."
  [code]
  (let [most-lines 12
        code-lines (string/split-lines (string/trim code))]
    (str (string/join "\n" (take most-lines code-lines))
         (when (> (count code-lines) most-lines) "\n…"))))

(defn prompt
  "A REPL tab's prompt for namespace `namespace-name`, the session's own,
  user, when nil."
  [namespace-name]
  (str (or namespace-name "user") "=> "))

(defn shell-prompt
  "A shell tab's prompt: the last folder of its working directory."
  [{:keys [cwd]}]
  (let [folder (when cwd (last (remove string/blank? (string/split cwd #"[/\\]"))))]
    (str (or folder "") (when folder " ") "$ ")))

(defn- remembered-input [history text]
  (let [kept-inputs 100]
    (vec (take-last kept-inputs (conj (filterv #(not= text %) history) text)))))

(defn- next-request [state]
  (let [number (get-in state [:repl :requests-made])]
    [(assoc-in state [:repl :requests-made] (inc number)) (str "eval-" number) number]))

;; Tabs

(defn- new-session [kind following]
  (cond-> {:kind kind :log [] :entries-made 0 :history [] :pending {} :message nil}
    (= :repl kind) (assoc :status :disconnected :port nil :ns (:ns following) :ns-path (:path following))
    (= :shell kind) (assoc :status :starting :label "Shell" :cwd nil)))

(defn- tab-number [tab]
  (if-let [[_ digits] (re-matches #"tab-(\d+)" (str tab))]
    #?(:clj (Long/parseLong digits) :cljs (js/parseInt digits 10))
    0))

(defn- with-tab
  "The state with tab `tab`, a `kind` session, among its tabs, made
  active, and the tab that evaluates when it is a REPL."
  [state tab kind]
  (cond-> (-> state
              (update-in [:repl :tabs-made] max (tab-number tab))
              (update-in [:repl :tabs] conj tab)
              (assoc-in [:repl :active] tab)
              (assoc-in (session-path tab) (new-session kind (get-in state [:repl :following]))))
    (= :repl kind) (assoc-in [:repl :evaluating] tab)))

(defn- opened-tab [state kind]
  (let [tab (str "tab-" (inc (get-in state [:repl :tabs-made])))]
    [(with-tab state tab kind) tab]))

(defn- ensure-tab
  "The state holding tab `tab` of `kind`, added when the host reports on a
  tab the view no longer knows, such as after the view reloaded."
  [state tab kind]
  (if (or (nil? tab) (session state tab))
    state
    (let [{:keys [active evaluating]} (:repl state)]
      (cond-> (with-tab state tab kind)
        active (assoc-in [:repl :active] active)
        (and evaluating (= :repl kind)) (assoc-in [:repl :evaluating] evaluating)))))

(defn- connecting [state tab message]
  (update-in state (session-path tab) assoc :status :connecting :message message))

(defn- open-pane [state]
  (update state :repl assoc :open? true :menu-open? false))

(defn- connect-tab
  "Connects REPL tab `tab` to the server the project's port file names,
  or to Calva's REPL when the tab joins Calva's."
  [state tab]
  (let [via (:via (session state tab))]
    (changed (connecting state tab (if (= :calva via) "Joining Calva's REPL…" "Looking for an nREPL server…"))
             (send-effect :view/repl-connect (cond-> {:tab tab} via (assoc :via via))))))

(defn new-repl-tab
  "Opens a REPL tab that connects to the server the project's port file
  names, or with `via` :calva joins Calva's REPL."
  ([state] (new-repl-tab state nil))
  ([state via]
   (let [[state tab] (opened-tab state :repl)]
     (connect-tab (cond-> (open-pane state) via (assoc-in (conj (session-path tab) :via) via)) tab))))

(defn new-shell-tab
  "Opens a shell tab in a new VS Code terminal, or in the open terminal
  `terminal-id` names."
  [state terminal-id]
  (let [[state tab] (opened-tab state :shell)]
    (changed (open-pane state)
             (send-effect :view/shell-open (cond-> {:tab tab} terminal-id (assoc :terminal terminal-id))))))

(defn toggle-repl
  "Opens or closes the pane. Opening it without a tab opens a REPL tab,
  and opening it on a disconnected REPL tab connects that tab."
  [state]
  (let [{:keys [open? active]} (:repl state)
        shown (active-session state)]
    (cond
      open? (changed (update state :repl assoc :open? false :menu-open? false))
      (nil? shown) (new-repl-tab state)
      (and (= :repl (:kind shown)) (= :disconnected (:status shown))) (connect-tab (open-pane state) active)
      :else (changed (open-pane state)))))

(defn- select-tab [state tab]
  (if (session state tab)
    (changed (cond-> (update state :repl assoc :active tab :menu-open? false)
               (= :repl (:kind (session state tab))) (assoc-in [:repl :evaluating] tab)))
    (unchanged state)))

(defn- close-tab
  "Closes tab `tab`, showing its neighbor, and asks the host to let go of
  its connection or terminal."
  [state tab]
  (if-not (session state tab)
    (unchanged state)
    (let [{:keys [tabs active evaluating sessions]} (:repl state)
          remaining (filterv #(not= tab %) tabs)
          position (.indexOf tabs tab)
          neighbor (get remaining (max 0 (dec position)))
          first-repl (some #(when (= :repl (get-in sessions [% :kind])) %) remaining)]
      (changed (-> state
                   (update-in [:repl :sessions] dissoc tab)
                   (update :repl assoc
                           :tabs remaining
                           :active (if (= tab active) neighbor active)
                           :evaluating (if (= tab evaluating) first-repl evaluating)))
               (send-effect :view/repl-close {:tab tab})))))

(defn- toggle-menu
  "Opens or closes the menu of new tabs, asking the host for VS Code's
  open terminals when it opens."
  [state]
  (let [opening? (not (get-in state [:repl :menu-open?]))]
    (changed (assoc-in state [:repl :menu-open?] opening?)
             (when opening? (send-effect :view/list-terminals {})))))

;; Typing into a tab

(defn- port-target [text]
  (when-let [[_ host port] (re-matches #"(?:([^\s:]+):)?(\d{1,5})" text)]
    (cond-> {:port #?(:clj (Long/parseLong port) :cljs (js/parseInt port 10))}
      host (assoc :host host))))

(defn- repl-submit
  "Sends what was typed into a REPL tab, a port to connect to while
  unconnected, else code for the tab's namespace, with its file so the
  host can load the ns form first."
  [state tab text]
  (let [code (string/trim text)
        {:keys [status ns ns-path via]} (session state tab)
        target (when (and (not= :connected status) (not= :calva via)) (port-target code))]
    (cond
      (string/blank? code) (unchanged state)

      target
      (changed (connecting state tab (str "Connecting to port " (:port target) "…"))
               (send-effect :view/repl-connect (assoc target :tab tab)))

      :else
      (let [[state id] (next-request state)]
        (changed (-> state
                     (update-in (conj (session-path tab) :history) remembered-input code)
                     (assoc-in (conj (session-path tab) :pending id) {:origin :pane :ns ns})
                     (with-log tab :input (str (prompt ns) code) id))
                 (send-effect :view/repl-eval (cond-> {:tab tab :id id :code code}
                                                ns (assoc :ns ns)
                                                ns-path (assoc :path ns-path))))))))

(defn- shell-submit
  "Runs a command typed into a shell tab. While a command runs, the line
  goes to it as input, as an answer to a prompt does."
  [state tab text]
  (let [command (string/trim text)
        current (session state tab)
        remembered (fn [state line]
                     (cond-> state
                       (not (string/blank? line)) (update-in (conj (session-path tab) :history) remembered-input line)))]
    (cond
      (contains? #{:closed :failed} (:status current))
      (unchanged (with-log state tab :err "This terminal is gone. Open another shell with +.\n" nil))

      (= :running (:status current))
      (changed (remembered state text)
               (send-effect :view/shell-input {:tab tab :text text}))

      (string/blank? command) (unchanged state)

      :else
      (let [[state id] (next-request state)]
        (changed (-> state
                     (remembered command)
                     (assoc-in (conj (session-path tab) :pending id) {:origin :shell})
                     (assoc-in (conj (session-path tab) :status) :running)
                     (with-log tab :input (str (shell-prompt current) command) id))
                 (send-effect :view/shell-run {:tab tab :id id :command command}))))))

(defn submit
  "Sends what was typed into the tab the pane shows."
  [state text]
  (let [tab (get-in state [:repl :active])]
    (case (:kind (session state tab))
      :repl (repl-submit state tab text)
      :shell (shell-submit state tab text)
      (unchanged state))))

(defn- interrupt
  "Stops what the shown tab runs, a REPL's evaluations or a shell's
  command, as Ctrl+C in a terminal does."
  [state]
  (let [tab (get-in state [:repl :active])
        {:keys [kind pending]} (session state tab)]
    (cond
      (and (= :repl kind) (seq pending)) (changed state (send-effect :view/repl-interrupt {:tab tab}))
      (= :shell kind) (changed state (send-effect :view/shell-interrupt {:tab tab}))
      :else (unchanged state))))

(defn- interrupt-evaluations
  "Stops the evaluations of every REPL tab, whichever tab shows."
  [state]
  (let [{:keys [tabs sessions]} (:repl state)
        running (filter (fn [tab]
                          (let [{:keys [kind pending]} (get sessions tab)]
                            (and (= :repl kind) (seq pending))))
                        tabs)]
    (apply changed state (map #(send-effect :view/repl-interrupt {:tab %}) running))))

(defn- connect-shown [state]
  (let [tab (get-in state [:repl :active])]
    (if (= :repl (:kind (session state tab)))
      (connect-tab state tab)
      (unchanged state))))

(defn- disconnect-shown [state]
  (let [tab (get-in state [:repl :active])]
    (if (= :repl (:kind (session state tab)))
      (changed state (send-effect :view/repl-disconnect {:tab tab}))
      (unchanged state))))

;; Following pages

(def ^:private clojure-dialects #{"clj" "cljs" "cljc" "bb"})

(defn- clojure-file [node]
  (some #(when (contains? clojure-dialects (:dialect %)) %) (:files node)))

(defn default-namespace
  "The namespace code from the Clojure file at `path` runs in when the
  file names none, as Clojure runs a script, cljs.user for a
  ClojureScript file and user for the rest."
  [path]
  (if (string/ends-with? (str path) ".cljs") "cljs.user" "user"))

(defn file-code
  "What loading the Clojure file at `path` sends, its text, preceded on
  its first line by an ns form for the default namespace when the file
  names none, so lines keep their numbers. nbb follows an ns form where
  in-ns would not move it."
  [path text]
  (if (forms-state/namespace-at (forms-state/read-forms (editing-state/text-lines text)) [0 0])
    text
    (str "(ns " (default-namespace path) ") " text)))

(defn ns-form
  "The ns form of a file's `text`, as {:code :line :column} with the line
  and column counted from one, when it names `namespace-name`; else nil."
  [text namespace-name]
  (let [lines (editing-state/text-lines text)
        forms (forms-state/read-forms lines)
        form (forms-state/namespace-form forms)
        [line column] (:start form)]
    (when (and form (= namespace-name (forms-state/namespace-at forms (:start form))))
      {:code (forms-state/form-text lines form) :line (inc line) :column (inc column)})))

(defn ns-form-note
  "The note telling a REPL tab that the ns form of the file at `path`
  runs before code for `namespace-name`, which the REPL had not loaded."
  [namespace-name path]
  (str "The REPL had not loaded " namespace-name ", so its ns form in "
       (peek (string/split (str path) #"/")) " goes first."))

(defn follow-page
  "The state following page `namespace-id` when it holds Clojure, its
  namespace becoming that of the REPL tab that evaluates and of tabs
  opened later."
  [state namespace-id]
  (let [node (get-in state [:snapshot :namespaces namespace-id])
        file (clojure-file node)
        tab (get-in state [:repl :evaluating])
        namespace-name (when file
                         (if (= :namespace (:kind node)) (:name node) (default-namespace (:path file))))]
    (if namespace-name
      (let [following {:namespace-id namespace-id :ns namespace-name :path (:path file)}]
        (cond-> (assoc-in state [:repl :following] following)
          (= :repl (:kind (session state tab)))
          (update-in (session-path tab) assoc :ns namespace-name :ns-path (:path file))))
      state)))

;; Evaluating from pages

(defn load-order
  "Paths of the Clojure files loading `namespace-id` with what it requires
  within the cloud, each once after its requires. Only files running
  where the namespace's own file runs count."
  [snapshot namespace-id]
  (let [nodes (:namespaces snapshot)
        root-dialect (:dialect (clojure-file (get nodes namespace-id)))
        runs-beside {"clj" ["clj" "cljc" "bb"]
                     "bb" ["bb" "clj" "cljc"]
                     "cljc" ["cljc" "clj"]
                     "cljs" ["cljs" "cljc"]}
        dialects (get runs-beside root-dialect [])
        loadable (fn [node]
                   (some (fn [dialect] (some #(when (= dialect (:dialect %)) (:path %)) (:files node))) dialects))
        visit (fn visit [[order seen] id]
                (if (contains? seen id)
                  [order seen]
                  (let [node (get nodes id)
                        path (when node (loadable node))]
                    (if (nil? path)
                      [order (conj seen id)]
                      (let [[order seen] (reduce visit [order (conj seen id)] (:requires node))]
                        [(conj order path) seen])))))]
    (first (visit [[] #{}] namespace-id))))

(defn evaluation
  "What evaluating `scope` sends from `buffer`, as {:code :start :end
  :namespace-name}, or a {:notice} saying why nothing can. The scopes
  follow Calva's commands; those to the caret close the brackets open
  there."
  [{:keys [lines caret language path] :as buffer} scope]
  (let [read-forms (delay (forms-state/read-forms lines))
        namespace-at (fn [position]
                       (or (forms-state/namespace-at @read-forms position) (default-namespace path)))
        whole (fn [form]
                (cond
                  (nil? form) {:notice "Put the caret in or beside a form to evaluate it."}
                  (not (:closed? form)) {:notice "The form at the caret is not closed."}
                  :else {:code (forms-state/form-text lines form) :start (:start form) :end (:end form)
                         :namespace-name (namespace-at (:start form))}))
        up-to (fn [start end]
                (let [code (str (string/trimr (forms-state/form-text lines {:start start :end end}))
                                (forms-state/closing-brackets @read-forms start end))]
                  (if (string/blank? code)
                    {:notice "Nothing comes before the caret to evaluate."}
                    {:code code :start start :end end :namespace-name (namespace-at start)})))
        [selection-start selection-end] (when (editing-state/selected? buffer) (editing-state/ordered buffer))]
    (cond
      (not= :clojure language)
      {:notice "Only Clojure files evaluate in the REPL."}

      (contains? #{:file :file-with-requires} scope)
      {:code (file-code path (string/join "\n" lines))
       :start [0 0] :end [0 0]
       :namespace-name (or (forms-state/namespace-at @read-forms [0 0]) (default-namespace path))}

      (and selection-start (= :form scope))
      {:code (editing-state/selected-text buffer) :start selection-start :end selection-end
       :namespace-name (namespace-at selection-start)}

      (and selection-start (= :to-caret scope))
      (up-to selection-start selection-end)

      :else
      (case scope
        :form (whole (forms-state/form-at @read-forms caret))
        :enclosing (whole (or (forms-state/enclosing-at @read-forms caret) (forms-state/top-level-at @read-forms caret)))
        :top-level (whole (forms-state/top-level-at @read-forms caret))
        :to-caret (if-let [enclosing (forms-state/enclosing-at @read-forms caret)]
                    (up-to (:start enclosing) caret)
                    {:notice "Put the caret inside a form to evaluate it up to the caret."})
        :top-level-to-caret (if-let [top (forms-state/top-level-at @read-forms caret)]
                              (up-to (:start top) caret)
                              {:notice "Put the caret in or beside a form to evaluate it."})
        :file-to-caret (up-to [0 0] caret)))))

(defn- pruned-results [results]
  (let [kept-results 24]
    (if (> (count results) kept-results)
      (into {} (take-last kept-results (sort-by (comp :order val) results)))
      results)))

(defn- evaluation-tab
  "The REPL tab code from pages goes to, the one last shown, else the
  first REPL tab, or nil when there is none."
  [state]
  (let [{:keys [evaluating tabs sessions]} (:repl state)]
    (if (= :repl (get-in sessions [evaluating :kind]))
      evaluating
      (some #(when (= :repl (get-in sessions [% :kind])) %) tabs))))

(defn- load-message
  "What loading the edited file sends for `scope`, the file alone or with
  its requires/dependencies."
  [state scope {:keys [tab id code path namespace-id namespace-name]}]
  (if (= :file scope)
    {:message (send-effect :view/repl-load-file {:tab tab :id id :code code :path path})
     :echo (str "Loading " path)}
    (let [order (load-order (:working-snapshot state) namespace-id)
          paths (if (seq order) order [path])
          required (dec (count paths))]
      {:message (send-effect :view/repl-load-files {:tab tab :id id :paths paths})
       :echo (str "Loading " (or namespace-name path)
                  (case required
                    0 ""
                    1 " and its requires/dependencies, 1 namespace"
                    (str " and its requires/dependencies, " required " namespaces")))})))

(defn- sent-evaluation
  "[state effect] sending `request`'s code to the REPL tab that evaluates,
  echoed in its log, opening a tab and the pane when there is none."
  [state {:keys [id origin scope code path namespace-id namespace-name line column result-key]}]
  (let [[state tab] (if-let [existing (evaluation-tab state)]
                      [state existing]
                      (let [[opened tab] (opened-tab state :repl)] [(open-pane opened) tab]))
        {:keys [message echo]} (if (contains? #{:file :file-with-requires} scope)
                                 (load-message state scope {:tab tab :id id :code code :path path
                                                            :namespace-id namespace-id :namespace-name namespace-name})
                                 {:message (send-effect :view/repl-eval {:tab tab :id id :code code :ns namespace-name
                                                                         :path path :line line :column column})
                                  :echo (echoed code)})]
    [(-> state
         (assoc-in (conj (session-path tab) :pending id)
                   (cond-> {:origin origin :ns namespace-name} result-key (assoc :result-key result-key)))
         (with-log tab :input (str (prompt namespace-name) echo) id))
     message]))

(defn evaluate
  "Sends the code `scope` takes from the file being edited to the REPL tab
  that evaluates. :file loads the file, and :file-with-requires loads
  what it requires within the cloud first."
  [state scope]
  (let [{:keys [path namespace-id]} (:editing state)
        buffer (get-in state [:buffers path])
        {:keys [notice code start end namespace-name]} (when buffer (evaluation buffer scope))]
    (cond
      (nil? buffer) (unchanged state)
      notice (unchanged (with-notice state notice))
      :else
      (let [[state id number] (next-request state)
            [start-line start-column] start
            [end-line end-column] end
            ;; A selection of whole lines ends at the start of the next.
            result-line (if (and (zero? end-column) (> end-line start-line)) (dec end-line) end-line)
            line-text (nth (:lines buffer) result-line)
            result-key [path result-line]
            [state message] (sent-evaluation state {:id id :origin :page :scope scope :code code :path path
                                                    :namespace-id namespace-id :namespace-name namespace-name
                                                    :line (inc start-line) :column (inc start-column)
                                                    :result-key result-key})]
        (changed (-> state
                     (assoc-in [:inline-results result-key]
                               {:id id
                                :order number
                                :namespace-id namespace-id
                                :path path
                                :line result-line
                                :column (editing-state/visual-column line-text (count line-text) page/tab-size)
                                :line-text line-text
                                :loading? (contains? #{:file :file-with-requires} scope)
                                :status :pending})
                     (update :inline-results pruned-results))
                 message)))))

(defn take-evaluation
  "The state after the host passed on an evaluation from one of VS Code's
  editors. The pane follows the file's page, and the code is echoed in
  the tab that evaluates."
  [state {:keys [id scope code path namespace-id namespace-name line column]}]
  (let [page-id (or namespace-id
                    (some (fn [[candidate node]] (when (some #(= path (:path %)) (:files node)) candidate))
                          (get-in state [:working-snapshot :namespaces])))
        [state message] (sent-evaluation (cond-> state page-id (follow-page page-id))
                                         {:id id :origin :editor :scope scope :code code :path path
                                          :namespace-id page-id :namespace-name namespace-name
                                          :line line :column column})]
    (changed state message)))

(defn- load-following
  "Loads the namespace the pane follows after everything it requires."
  [state]
  (let [{:keys [namespace-id ns path]} (get-in state [:repl :following])
        tab (get-in state [:repl :active])]
    (if (and namespace-id (= :repl (:kind (session state tab))))
      (let [[state id] (next-request state)
            order (load-order (:working-snapshot state) namespace-id)
            paths (if (seq order) order [path])]
        (changed (-> state
                     (assoc-in (conj (session-path tab) :pending id) {:origin :pane :ns ns})
                     (with-log tab :input (str (prompt ns) "Loading " ns " and its requires/dependencies") id))
                 (send-effect :view/repl-load-files {:tab tab :id id :paths paths})))
      (unchanged (with-notice state "Click a Clojure page first, so the REPL knows which namespace to load.")))))

;; Replies

(defn error-summary
  "One line saying what went wrong in an evaluation's error output, its
  last line, or for an error Clojure prints as null, the error's class.
  Nil without output."
  [err ex]
  (let [lines (remove string/blank? (string/split-lines (or err "")))
        error-line (last lines)
        class-name (fn [text]
                     (when text
                       (or (second (re-find #"\(([\w.$]+)\) at " text))
                           (second (re-find #"(?:class )?(?:[\w$]+\.)*([\w$]+)$" (string/trim text))))))]
    (cond
      (nil? error-line) (some-> ex class-name)
      (contains? #{"null" "nil"} (string/trim error-line)) (or (class-name (last (butlast lines)))
                                                               (some-> ex class-name)
                                                               error-line)
      :else error-line)))

(defn- finished-result
  "An inline result once its evaluation ended, keeping its value, or else
  saying what went wrong, as `words`, the last reply's status, or the
  error output tells it."
  [{:keys [status err ex] :as result} words]
  (if (= :value status)
    result
    (assoc result
           :status :error
           :text (cond
                   (contains? words "interrupted") "Interrupted"
                   (contains? words "namespace-not-found") "Namespace not loaded"
                   (contains? words "not-connected") "Not connected"
                   (contains? words "connection-closed") "Connection closed"
                   :else (or (error-summary err ex) "No value")))))

(defn answered-result
  "A result after one more reply, `words` being its status words. A result
  holds :status, :pending, :value, or :error, its :text, and its :err
  output; an error outweighs a value."
  [result {:keys [err value ex]} words]
  (let [longest-error 2000]
    (cond-> result
      value (assoc :status :value :text value)
      err (update :err #(let [joined (str % err)]
                          (subs joined (max 0 (- (count joined) longest-error)))))
      ex (assoc :ex ex)
      (or ex (contains? words "eval-error")) (assoc :status :error)
      (contains? words "done") (finished-result words))))

(defn take-repl-status
  "The state with a REPL tab's connection status, noting each change in
  its log. A failed connection opens the pane, which says why."
  [state {:keys [tab status port message via]}]
  (let [state (ensure-tab state tab :repl)
        {previous-status :status previous-message :message} (session state tab)]
    (cond-> (update-in state (session-path tab) assoc :status status :port port :message message)
      via (assoc-in (conj (session-path tab) :via) via)
      (and message (not= [status message] [previous-status previous-message])) (with-log tab :info message nil)
      (= :failed status) (assoc-in [:repl :open?] true))))

(defn- unresolved-hint
  "A note for an unresolved symbol, saying that loading its file defines
  it when the code's namespace does, else naming the cloud's namespaces
  defining it, or nil."
  [state request-ns err]
  (when-let [[_ symbol-text] (re-find #"(?:Unable to resolve symbol|Could not resolve symbol|Use of undeclared Var):? ([^\s,]+)" err)]
    (let [symbol-name (subs symbol-text (inc (or (string/last-index-of symbol-text "/") -1)))
          most-named 3
          nodes (get-in state [:working-snapshot :namespaces])
          defines? (fn [node] (some #(= symbol-name (:name %)) (:definitions node)))
          own (get nodes request-ns)]
      (if (and own (defines? own))
        (str symbol-name " is defined in " (:path (clojure-file own)) ", which this REPL has not loaded. "
             "Ctrl+Alt+C Enter on its page loads it, as does the load button above.")
        (let [definers (sort (keep (fn [[id node]] (when (and (= :namespace (:kind node)) (defines? node)) id)) nodes))]
          (when (seq definers)
            (str symbol-name " is defined in " (string/join ", " (take most-named definers))
                 (when (> (count definers) most-named) ", and more") ".")))))))

(defn take-repl-output
  "The state after one reply to a REPL request, its output logged, a hint
  for a symbol the cloud defines, and its result on its page. A reply to
  the pane's own request moves the tab into its namespace."
  [state {:keys [tab id input out err value ns note] :as reply}]
  (let [{:keys [origin result-key] request-ns :ns} (get-in state (conj (session-path tab) :pending id))
        words (set (:status reply))
        hint (when err (unresolved-hint state request-ns err))
        moved? (and (= :pane origin) (not (string/blank? ns)) (not= ns (:ns (session state tab))))]
    (cond-> state
      note (with-log tab :info note id)
      input (with-log tab :input (str (prompt ns) input) id)
      out (with-log tab :out out id)
      err (with-log tab :err err id)
      hint (with-log tab :info hint id)
      value (with-log tab :value value id)
      moved? (update-in (session-path tab) assoc
                        :ns ns
                        :ns-path (some-> (get-in state [:working-snapshot :namespaces ns]) clojure-file :path))
      (contains? words "namespace-not-found")
      (with-log tab :err (str "The REPL has not loaded " (or request-ns "that namespace") " yet. Load its file first.") id)
      (contains? words "interrupted") (with-log tab :info "Interrupted." id)
      (and result-key (= id (get-in state [:inline-results result-key :id])))
      (update-in [:inline-results result-key] answered-result reply words)
      (contains? words "done") (update-in (conj (session-path tab) :pending) dissoc id))))

(defn take-shell-status
  "The state with a shell tab's terminal status, its label, and its
  working directory, noting each new message in its log."
  [state {:keys [tab status label cwd message]}]
  (let [state (ensure-tab state tab :shell)
        previous (:message (session state tab))]
    (cond-> (update-in state (session-path tab) merge
                       (cond-> {:status status :message message}
                         label (assoc :label label)
                         cwd (assoc :cwd cwd)))
      (and message (not= message previous)) (with-log tab :info message nil)
      (contains? #{:closed :failed} status) (assoc-in (conj (session-path tab) :pending) {}))))

(defn take-shell-output
  "The state after output from a shell tab's terminal. A command typed in
  VS Code's own terminal shows as input with its first output, and its
  end reports a failing exit code and the new directory."
  [state {:keys [tab id command text note exit-code done? cwd]}]
  (let [state (ensure-tab state tab :shell)]
    (cond-> state
      note (with-log tab :info note id)
      command (-> (with-log tab :input (str (shell-prompt (session state tab)) command) id)
                  (update-in (session-path tab) (fn [current]
                                                  (cond-> (assoc-in current [:pending id] {:origin :panel})
                                                    (= :ready (:status current)) (assoc :status :running)))))
      (seq text) (with-log tab :out text id)
      (and done? exit-code (not (zero? exit-code))) (with-log tab :info (str "Exited with code " exit-code ".") id)
      cwd (assoc-in (conj (session-path tab) :cwd) cwd)
      done? (update-in (session-path tab) (fn [current]
                                            (let [remaining (dissoc (:pending current) id)]
                                              (cond-> (assoc current :pending remaining)
                                                (and (= :running (:status current)) (empty? remaining)) (assoc :status :ready))))))))

(defn take-terminals
  "The state with VS Code's open terminals, offered in the menu of new
  tabs, and whether Calva is there to join."
  [state {:keys [terminals calva?]}]
  (update state :repl assoc :terminals (vec terminals) :calva? (boolean calva?)))

;; The pane's place and size

(defn valid-geometry
  "`geometry` when it is a {:left :top :width :height} map of numbers,
  otherwise nil."
  [geometry]
  (when (and (map? geometry) (every? #(number? (get geometry %)) [:left :top :width :height]))
    (select-keys geometry [:left :top :width :height])))

(defn- save-geometry [state geometry]
  (if-let [kept (valid-geometry geometry)]
    (changed (assoc-in state [:repl :geometry] kept)
             (send-effect :view/save-preference {:key :repl-geometry :value kept}))
    (unchanged state)))

;; Handlers

(def handlers
  "Each REPL event's handler, by the event's first element."
  {:toggle-repl toggle-repl
   :repl-new-tab (fn [state kind & [terminal-id]]
                   (case kind
                     :shell (new-shell-tab state terminal-id)
                     :calva (new-repl-tab state :calva)
                     (new-repl-tab state)))
   :repl-select-tab select-tab
   :repl-close-tab close-tab
   :repl-menu toggle-menu
   :repl-connect connect-shown
   :repl-disconnect disconnect-shown
   :repl-submit submit
   :repl-interrupt interrupt
   :interrupt-evaluations interrupt-evaluations
   :repl-clear (fn [state]
                 (if-let [tab (get-in state [:repl :active])]
                   (unchanged (assoc-in state (conj (session-path tab) :log) []))
                   (unchanged state)))
   :repl-load-following load-following
   :save-repl-geometry save-geometry
   :evaluate evaluate})

(def message-handlers
  "Each REPL message's handler that changes only the state, by the host
  message's :type."
  {:host/repl-status take-repl-status
   :host/repl-output take-repl-output
   :host/shell-status take-shell-status
   :host/shell-output take-shell-output
   :host/terminals take-terminals})

(def sending-message-handlers
  "Each REPL message's handler that sends messages of its own, returning
  {:state :effects}, by the host message's :type."
  {:host/evaluate take-evaluation})
