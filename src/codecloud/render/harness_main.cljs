(ns codecloud.render.harness-main
  "The browser harness, standing in for VS Code. It loads a data file from
  `node dist/cli.js bundle` and answers the view's messages. Query
  parameters set what the extension would: data names the file in
  dev/data, theme, sky, display, arrange, headers, color, totals, spread,
  and editor-colors set the first settings, arrive with arrive-after
  starts at path:line as zooming out of a file does, visitor sends a bird
  or airplane across, and repl set to missing acts out a project without
  an nREPL server. Scripts drive it through window.codecloudDispatch, and
  read it through codecloudScreenOf, codecloudPointOf, and
  codecloudStateAt."
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.render.app :as app]
            [codecloud.render.dom :as dom]
            [codecloud.render.editing :as editing]
            [codecloud.render.pages :as pages]
            [codecloud.render.sky :as sky]))

(defn- query-parameter [parameter-name]
  (.get (js/URLSearchParams. (.-search js/location)) parameter-name))

(defn- show-toast!
  "Shows what the extension would have done, since the harness has no editor."
  [message]
  (let [^js toast (dom/element [:p {:style {:position "fixed" :left "50%" :bottom "96px"
                                            :transform "translateX(-50%)" :padding "8px 14px"
                                            :border-radius "12px" :background "rgba(20,30,50,0.85)"
                                            :color "white" :font "550 13px Recursive, sans-serif"
                                            :z-index "10"}}
                                message])]
    (.appendChild (.-body js/document) toast)
    (js/setTimeout #(.remove toast) 2600)))

(defn- load-bundle []
  (let [data-name (or (query-parameter "data") "orchard")]
    (-> (js/fetch (str "data/" data-name ".edn"))
        (.then (fn [^js response] (.text response)))
        (.then reader/read-string))))

(defn- remembered [preference-key]
  (try
    (some-> (.getItem js/localStorage (str "codecloud:" (name preference-key))) reader/read-string)
    (catch :default _error nil)))

(defn- layers-parameter
  "The layers the comma-separated color parameter names, none for no
  layers, or nil without the parameter."
  []
  (when-let [named (query-parameter "color")]
    (into [] (comp (remove #(contains? #{"" "none"} %)) (map keyword)) (string/split named #","))))

(defn- later!
  "Delivers `reply` after `delay` milliseconds, as a host's answers arrive."
  [deliver! delay reply]
  (js/setTimeout #(deliver! reply) delay))

(def ^:private sample-theme
  "A dark editor theme standing in for the editor's, its colors invented
  for the harness."
  {:theme-id "Harness Night"
   :dark? true
   :colors {"editor.background" "#141a24" "editor.foreground" "#d8dee9" "editorLineNumber.foreground" "#5c6a80"}
   :token-colors [{"scope" "comment" "settings" {"foreground" "#7f8ea3" "fontStyle" "italic"}}
                  {"scope" "string" "settings" {"foreground" "#a3d9a5"}}
                  {"scope" "keyword" "settings" {"foreground" "#f0a3c8"}}
                  {"scope" "constant" "settings" {"foreground" "#ffc58a"}}
                  {"scope" "entity.name.function" "settings" {"foreground" "#8ecbff" "fontStyle" "bold"}}
                  {"scope" "support.type" "settings" {"foreground" "#9fe3dc"}}]
   :bracket-pairs? true})

(defn- init-message
  "The settings and preferences the extension would send first, from the
  query string and what this browser remembers."
  [bundle]
  {:type :host/init
   :settings {:zoom-out-threshold 150
              :editor-colors? (or (= "on" (query-parameter "editor-colors")) (true? (remembered :editor-colors?)))}
   :theme-kind (if (= "light" (query-parameter "theme")) :light :dark)
   :sky-mode (keyword (or (query-parameter "sky") "auto"))
   :display-mode (keyword (or (query-parameter "display") "code"))
   :view-filter (remembered :view-filter)
   :arrangement (or (some-> (query-parameter "arrange") keyword) (remembered :arrangement))
   :header-mode (or (some-> (query-parameter "headers") keyword) (remembered :header-mode))
   :color-layers (or (layers-parameter) (remembered :color-layers))
   :color-overrides (remembered :color-overrides)
   :totals-open? (or (= "open" (query-parameter "totals")) (remembered :totals-open?))
   :tint-clouds? (remembered :tint-clouds?)
   :inspector-minimized? (remembered :inspector-minimized?)
   :spread (or (some-> (query-parameter "spread") js/parseFloat) (remembered :spread))
   :cloud-name (:root-name bundle)
   :editor-font {:family nil :size 14 :line-height 0}})

(defn- source-files
  "A page's files for `source-key`. The bundle holds only working-tree
  text, so other snapshots come back empty."
  [bundle namespace-id source-key]
  (let [working-files (get-in bundle [:sources namespace-id] [])]
    (if (= source-key (ui-state/source-key (:snapshot bundle) namespace-id))
      working-files
      (mapv #(assoc % :text "") working-files))))

(defmulti ^:private answer!
  "Replies to one view message from the loaded bundle, the way the
  extension would."
  (fn [_bundle message _deliver!] (:type message)))

(defmethod answer! :default [_bundle _message _deliver!]
  nil)

(defmethod answer! :view/ready [bundle _message deliver!]
  (deliver! (init-message bundle))
  (later! deliver! 40 {:type :host/editor-colors :code-colors (code-colors/theme-colors sample-theme)})
  (deliver! {:type :host/views :views (:views bundle {}) :color-layers (:color-layers bundle [])})
  (deliver! {:type :host/status :message "Reading the codebase" :progress 0.6})
  (later! deliver! 150 {:type :host/history :commits (:commits bundle) :available? (boolean (seq (:commits bundle)))})
  (later! deliver! 200 {:type :host/snapshot :snapshot (:snapshot bundle)})
  (later! deliver! 260 {:type :host/status :message nil})
  (when-let [arrival (query-parameter "arrive")]
    (let [[path line] (string/split arrival #":")
          delay (or (some-> (query-parameter "arrive-after") (js/parseInt 10)) 1600)]
      (later! deliver! delay {:type :host/arrive :path path :line (js/parseInt line 10) :text-scale 0.67}))))

(defmethod answer! :view/select-snapshot [bundle {:keys [snapshot-id]} deliver!]
  (later! deliver! 120 {:type :host/snapshot
                        :snapshot (if (= :working-tree snapshot-id)
                                    (:snapshot bundle)
                                    (get-in bundle [:commit-snapshots snapshot-id]))}))

(defmethod answer! :view/request-sources [bundle {:keys [snapshot-id namespaces]} deliver!]
  (later! deliver! 60 {:type :host/sources
                       :snapshot-id snapshot-id
                       :entries (mapv (fn [{:keys [namespace-id source-key]}]
                                        {:namespace-id namespace-id
                                         :source-key source-key
                                         :files (source-files bundle namespace-id source-key)})
                                      namespaces)}))

(defmethod answer! :view/open-location [_bundle {:keys [path line]} _deliver!]
  (show-toast! (str "The extension would open " path " at line " line ".")))

(defonce ^:private edited-texts (atom {}))

(defn- bundle-text
  "The working tree text of the file at `path` in the bundle."
  [bundle path]
  (some #(when (= path (:path %)) (:text %)) (apply concat (vals (:sources bundle)))))

(defmethod answer! :view/edit [bundle {:keys [path revision text] [line column end-line end-column] :range} deliver!]
  (let [lines (string/split (or (get @edited-texts path) (bundle-text bundle path) "") #"\n" -1)
        offset (fn [target-line target-column]
                 (+ target-column (reduce + 0 (map #(inc (count %)) (take target-line lines)))))
        joined (string/join "\n" lines)
        updated (str (subs joined 0 (offset line column)) text (subs joined (offset end-line end-column)))]
    (swap! edited-texts assoc path updated)
    (later! deliver! 30 {:type :host/edited :path path :revision revision :content-key (str "edited:" revision) :dirty? true})))

(defmethod answer! :view/save [_bundle {:keys [path]} deliver!]
  (show-toast! (str "The extension would save " path "."))
  (later! deliver! 60 {:type :host/dirty :path path :dirty? false}))

(defmethod answer! :view/save-view [_bundle message _deliver!]
  (show-toast! (str "The extension would save the view " (pr-str (:name message)) " in the cloud file.")))

(defmethod answer! :view/save-preference [_bundle {preference :key :keys [value]} _deliver!]
  (try (.setItem js/localStorage (str "codecloud:" (name preference)) (pr-str value))
       (catch :default _error nil)))

;; A REPL stand-in. It evaluates a small part of Clojure in the browser,
;; enough to show results and errors the way an nREPL server sends them.
;; The repl parameter set to missing makes it a project without a server.

(defonce ^:private repl-namespace (atom "user"))

(defonce ^:private repl-vars (atom {}))

(defonce ^:private printed-output (atom ""))

(deftype ^:private HarnessVar [qualified-name]
  IPrintWithWriter
  (-pr-writer [_ writer _options] (-write writer (str "#'" qualified-name))))

(defn- qualified-name [var-symbol]
  (if (namespace var-symbol) (str var-symbol) (str @repl-namespace "/" var-symbol)))

(defn- printed-line [& values]
  (swap! printed-output str (string/join " " (map #(if (nil? %) "nil" (str %)) values)) "\n")
  nil)

(def ^:private harness-core
  {'+ + '- - '* * 'inc inc 'dec dec 'str str 'range range 'map map 'mapv mapv 'filter filter 'reduce reduce
   'count count 'first first 'rest rest 'last last 'conj conj 'assoc assoc 'update update 'get get 'merge merge
   'vector vector 'list list 'vec vec 'set set 'keys keys 'vals vals 'keyword keyword 'name name 'apply apply
   '= = '< < '> > '<= <= '>= >= 'even? even? 'odd? odd? 'take take 'drop drop 'reverse reverse 'sort sort
   'into into 'max max 'min min 'frequencies frequencies 'zipmap zipmap 'identity identity
   'println printed-line
   '/ (fn [numerator & divisors]
        (if (some zero? divisors) (throw (ex-info "Divide by zero" {})) (apply / numerator divisors)))})

(declare harness-eval)

(defn- harness-fn [locals parameters body]
  (fn [& values]
    (let [scope (merge locals (zipmap parameters values))]
      (last (map #(harness-eval scope %) body)))))

(defn- harness-eval [locals form]
  (cond
    (symbol? form) (cond
                     (contains? locals form) (get locals form)
                     (contains? @repl-vars (qualified-name form)) (get @repl-vars (qualified-name form))
                     (contains? harness-core form) (get harness-core form)
                     :else (throw (ex-info (str "Unable to resolve symbol: " form " in this context") {})))
    (vector? form) (mapv #(harness-eval locals %) form)
    (map? form) (into {} (map (fn [[entry-key entry-value]] [(harness-eval locals entry-key) (harness-eval locals entry-value)])) form)
    (set? form) (into #{} (map #(harness-eval locals %)) form)
    (and (seq? form) (seq form))
    (let [[head & arguments] form]
      (case head
        quote (first arguments)
        ns (do (reset! repl-namespace (str (first arguments))) nil)
        in-ns (do (reset! repl-namespace (str (harness-eval locals (first arguments)))) nil)
        comment nil
        def (let [[var-symbol value] arguments]
              (swap! repl-vars assoc (qualified-name var-symbol) (harness-eval locals value))
              (HarnessVar. (qualified-name var-symbol)))
        defn (let [[fn-symbol & more] arguments
                   [parameters & body] (if (string? (first more)) (rest more) more)]
               (swap! repl-vars assoc (qualified-name fn-symbol) (harness-fn locals parameters body))
               (HarnessVar. (qualified-name fn-symbol)))
        fn (harness-fn locals (first arguments) (rest arguments))
        let (let [[bindings & body] arguments
                  scope (reduce (fn [scope [bound value]] (assoc scope bound (harness-eval scope value)))
                                locals (partition 2 bindings))]
              (last (map #(harness-eval scope %) body)))
        if (let [[test then otherwise] arguments]
             (if (harness-eval locals test) (harness-eval locals then) (harness-eval locals otherwise)))
        do (last (map #(harness-eval locals %) arguments))
        (let [function (harness-eval locals head)]
          (when-not (ifn? function)
            (throw (ex-info (str (pr-str function) " cannot be called") {})))
          (apply function (map #(harness-eval locals %) arguments)))))
    :else form))

(defn- harness-replies
  "The replies an nREPL server would send evaluating `code` for request
  `id` in namespace `ns`: printed output and a value for each form, or an
  error stopping at the form that failed, then done."
  [id code ns]
  (when ns (reset! repl-namespace ns))
  (let [shown-items 100
        forms (try (reader/read-string (str "[" code "\n]"))
                   (catch :default error error))]
    (conj (if (instance? js/Error forms)
            [{:id id :err (str "Syntax error reading source at (REPL:1).\n" (ex-message forms) "\n")}
             {:id id :ex "class clojure.lang.ExceptionInfo" :status ["eval-error"]}]
            (loop [remaining forms
                   replies []]
              (if-let [form (first remaining)]
                (let [_ (reset! printed-output "")
                      outcome (try {:value (harness-eval {} form)} (catch :default error {:error error}))
                      printed (when (seq @printed-output) [{:id id :out @printed-output}])]
                  (if-let [error (:error outcome)]
                    (into replies (concat printed
                                          [{:id id :err (str "Execution error at " @repl-namespace "/eval (REPL:1).\n"
                                                             (ex-message error) "\n")}
                                           {:id id :ex "class clojure.lang.ExceptionInfo" :status ["eval-error"]}]))
                    (recur (rest remaining)
                           (into replies (concat printed
                                                 [{:id id :ns @repl-namespace
                                                   :value (binding [*print-length* shown-items] (pr-str (:value outcome)))}])))))
                replies)))
          {:id id :status ["done"]})))

(defn- deliver-replies!
  "Delivers replies a little apart, as a server's arrive."
  [deliver! replies]
  (doseq [[index reply] (map-indexed vector replies)]
    (later! deliver! (+ 80 (* 30 index)) (assoc reply :type :host/repl-output))))

(defmethod answer! :view/repl-connect [_bundle {:keys [port]} deliver!]
  (let [shown-port (or port 7888)]
    (if (and (nil? port) (= "missing" (query-parameter "repl")))
      (later! deliver! 120 {:type :host/repl-status :status :failed
                            :message "No nREPL port file in this folder or a folder above it. Start an nREPL server there, or type its port and press Enter."})
      (do (later! deliver! 100 {:type :host/repl-status :status :connecting :port shown-port
                                :message (str "Connecting to port " shown-port "…")})
          (later! deliver! 450 {:type :host/repl-status :status :connected :port shown-port
                                :message (str "Harness REPL on port " shown-port)})))))

(defmethod answer! :view/repl-disconnect [_bundle _message deliver!]
  (later! deliver! 60 {:type :host/repl-status :status :disconnected :message "Disconnected."}))

(defmethod answer! :view/repl-eval [_bundle {:keys [id code ns]} deliver!]
  (deliver-replies! deliver! (harness-replies id code ns)))

(defmethod answer! :view/repl-load-file [_bundle {:keys [id code]} deliver!]
  (let [replies (harness-replies id code nil)
        answered (butlast replies)]
    ;; Loading a file answers with the value of its last form alone.
    (deliver-replies! deliver! (concat (remove :value answered) (take-last 1 (filter :value answered)) [(last replies)]))))

(defn- harness-transport []
  (let [handlers (atom [])
        deliver! (fn [reply] (doseq [handler @handlers] (handler reply)))
        bundle (load-bundle)]
    {:subscribe! (fn [handler] (swap! handlers conj handler))
     :send! (fn [message] (.then bundle (fn [loaded] (answer! loaded message deliver!))))
     :view-state nil}))

(defn init! []
  (let [started (app/start! (js/document.getElementById "codecloud") (harness-transport))
        visitor-delay 1500]
    (set! (.-codecloudApp js/window) started)
    (set! (.-codecloudDispatch js/window) (fn [event-text] ((:dispatch! started) (reader/read-string event-text))))
    (set! (.-codecloudScreenOf js/window)
          (fn [namespace-id]
            (when-let [{:keys [screen-x screen-y half-width-pixels half-height-pixels]}
                       (pages/page-of (:page-layer started) namespace-id)]
              #js [screen-x screen-y half-width-pixels half-height-pixels])))
    (set! (.-codecloudPointOf js/window)
          (fn [namespace-id path line visual-column]
            (when-let [page-object (pages/page-of (:page-layer started) namespace-id)]
              (some-> (editing/line-screen-point page-object path line visual-column) clj->js))))
    (set! (.-codecloudStateAt js/window)
          (fn [path-text] (pr-str (get-in @(:state started) (reader/read-string path-text)))))
    (set! (.-codecloudTurn js/window)
          (fn [azimuth polar]
            (let [^js spherical (:spherical (:controller started))]
              (set! (.-theta spherical) azimuth)
              (set! (.-phi spherical) polar))))
    (when-let [visitor (query-parameter "visitor")]
      (js/setTimeout #(sky/send-for! (:sky started) (keyword visitor)) visitor-delay))))

(init!)
