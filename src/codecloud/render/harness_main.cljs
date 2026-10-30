(ns codecloud.render.harness-main
  "The browser harness, standing in for VS Code. It loads a data file from
  `node dist/cli.js bundle` and answers the view's messages. The README
  lists its query parameters."
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.render.app :as app]
            [codecloud.render.dom :as dom]
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
   :color-layers (or (layers-parameter) (remembered :color-layers))
   :color-overrides (remembered :color-overrides)
   :totals-open? (or (= "open" (query-parameter "totals")) (remembered :totals-open?))
   :tint-clouds? (remembered :tint-clouds?)
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
            (when-let [{:keys [screen-x screen-y]} (pages/page-of (:page-layer started) namespace-id)]
              #js [screen-x screen-y])))
    (set! (.-codecloudTurn js/window)
          (fn [azimuth polar]
            (let [^js spherical (:spherical (:controller started))]
              (set! (.-theta spherical) azimuth)
              (set! (.-phi spherical) polar))))
    (when-let [visitor (query-parameter "visitor")]
      (js/setTimeout #(sky/send-for! (:sky started) (keyword visitor)) visitor-delay))))

(init!)
