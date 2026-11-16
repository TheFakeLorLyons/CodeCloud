(ns codecloud.render.harness-main
  "The browser harness, standing in for VS Code. It loads a bundle from
  `node dist/cli.js bundle` and acts out the host's answers, a REPL,
  shells, agents, formatting, the assistant, and what the cloud leaves
  out. Query parameters set
  what the extension would, such as data, theme, sky, spaces, and
  arrange, and scripts drive the view through window.codecloudDispatch
  and codecloudDeliver and read it through codecloudStateAt,
  codecloudScreenOf, codecloudPointOf, and codecloudFlying."
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [codecloud.model.ignored :as ignored]
            [codecloud.model.metrics :as metrics]
            [codecloud.model.new-file :as new-file]
            [codecloud.presentation.clojure-format :as clojure-format]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.environments :as environments]
            [codecloud.presentation.forms-state :as forms-state]
            [codecloud.presentation.page-commands :as page-commands]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.render.app :as app]
            [codecloud.render.backdrop :as backdrop]
            [codecloud.render.dom :as dom]
            [codecloud.render.editing-view :as editing-view]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.pages :as pages]))

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

(defn- layers-parameter []
  (when-let [named (query-parameter "color")]
    (into [] (comp (remove #(contains? #{"" "none"} %)) (map keyword)) (string/split named #","))))

(defn- later! [deliver! milliseconds reply]
  (js/setTimeout #(deliver! reply) milliseconds))

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

(def ^:private demo-spaces
  "Two spaces written as the codecloud.spaces setting holds them, one room
  and one maze, for trying described spaces with the spaces parameter set
  to demo."
  [{"name" "Greenhouse" "layout" "room" "light" "day" "folders" "discs"
    "walls" {"color" "#dbe9d3" "pattern" "grid" "patternColor" "#9dbb94"}
    "floor" {"color" "#6d5a45" "pattern" "checks" "patternColor" "#5b4a39"}
    "ceiling" {"color" "#f1f7ef" "pattern" "grid" "patternColor" "#c3d6bf"}
    "lamps" "#fffbe8" "haze" "#cfe0c8"}
   {"name" "Gallery" "layout" "maze" "light" "day"
    "walls" {"color" "#f4f2ee" "pattern" "plain"}
    "floor" {"color" "#b9a58a" "pattern" "tiles" "patternColor" "#a48f74" "patternScale" 2}
    "ceiling" {"color" "#fbfaf7" "pattern" "grid" "patternColor" "#e2ded6"}
    "lamps" "#ffffff" "haze" "#e8e4dc"}])

(def ^:private demo-page-commands
  "Commands for every page's right-click menu, written as the
  codecloud.pageCommands setting holds them, for trying page commands
  with the commands parameter set to demo."
  (into []
        (keep page-commands/from-setting)
        [{"title" "Clay: show this file"
          "command" "calva.runCustomREPLCommand"
          "args" {"snippet" "(scicloj.clay.v2.api/make! {:source-path \"$file\"})"}
          "window" "8765/test-resources/windows/page.html"}
         {"title" "Note the place" "shell" "echo ${relativeFile}:${line}"}]))

(defn- init-message
  "The settings and preferences the extension would send first, from the
  query string and what this browser remembers."
  [bundle]
  (let [spaces (environments/spaces-from-setting (when (= "demo" (query-parameter "spaces")) demo-spaces))]
    {:type :host/init
     :settings {:zoom-out-threshold 150
                :editor-colors? (or (= "on" (query-parameter "editor-colors")) (true? (remembered :editor-colors?)))
                :toolbar-colors (case (query-parameter "toolbar")
                                  "plain" :plain
                                  "white" {"all" "#ffffff"}
                                  (or (remembered :toolbar-colors) :colored))
                :page-commands (when (= "demo" (query-parameter "commands")) demo-page-commands)}
     :theme-kind (if (= "light" (query-parameter "theme")) :light :dark)
     :sky-mode (environments/chosen-id (or (query-parameter "sky") (some-> (remembered :sky-mode) name) "auto") spaces)
     :simple-color (remembered :simple-color)
     :spaces spaces
     :display-mode (keyword (or (query-parameter "display") "code"))
     :view-filter (remembered :view-filter)
     :arrangement (or (some-> (query-parameter "arrange") keyword) (remembered :arrangement))
     :header-mode (or (some-> (query-parameter "headers") keyword) (remembered :header-mode))
     :color-layers (or (layers-parameter) (remembered :color-layers))
     :color-overrides (remembered :color-overrides)
     :totals-open? (or (= "open" (query-parameter "totals")) (remembered :totals-open?))
     :title-summary-hidden? (remembered :title-summary-hidden?)
     :auto-hide-bars? (remembered :auto-hide-bars?)
     :windows (remembered :windows)
     :grid (remembered :grid)
     :shown-ignored (remembered :shown-ignored)
     :tint-clouds? (remembered :tint-clouds?)
     :inspector-minimized? (remembered :inspector-minimized?)
     :repl-geometry (remembered :repl-geometry)
     :dock (remembered :dock)
     :spread (or (some-> (query-parameter "spread") js/parseFloat) (remembered :spread))
     :cloud-name (:root-name bundle)
     :editor-font {:family nil :size 14 :line-height 0}}))

(defonce ^:private made-files
  (atom {}))

(def ^:private ignored-stand-in
  "What the harness says the cloud leaves out, by folder, as the extension
  would list it."
  {"" [{:path ".cpcache/" :folder? true :reason :cloud}
       {:path "target/" :folder? true :reason :git}
       {:path ".gitignore" :folder? false :reason :type}
       {:path "notes.md" :folder? false :reason :type}]
   "target/" [{:path "target/classes/" :folder? true}
              {:path "target/generated.clj" :folder? false}]})

(def ^:private generated-text
  "(ns orchard.generated)\n\n(defn built\n  \"Made by the build, so git ignores it.\"\n  []\n  :at-build-time)\n")

(defn- generated-node
  "The page of target/generated.clj, a file git ignores, as the analyzer
  reads it once the viewer chooses to see it."
  []
  (let [lines (string/split-lines generated-text)
        node {:definitions [{:name "built" :kind :function :private? false :line 3 :end-line 6
                             :path "target/generated.clj" :inbound-count 0 :local-count 0}]
              :parse-error? false :name "orchard.generated" :cycle-id nil :external-requires [] :hotspots []
              :requires [] :id "orchard.generated" :kind :namespace :required-by [] :ignored? true
              :files [{:path "target/generated.clj" :dialect "clj" :line-count (count lines)
                       :longest-line (apply max (map count lines)) :content-key "generated" :parse-error nil :ignored? true}]
              :line-count (count lines) :method-count 0
              :doc nil :flag-counts {}
              :importance 0.1}]
    (assoc node :metrics (metrics/namespace-metrics node))))

(defn- generated-shown? []
  ((ignored/shown-predicate (ignored/chosen-paths (remembered :shown-ignored))) "target/generated.clj"))

(defn- working-snapshot
  "The bundle's working tree with the pages of the files made since, and
  the page of the ignored file once the viewer chooses to see it."
  [bundle]
  (cond-> (update (:snapshot bundle) :namespaces merge (update-vals @made-files :node))
    (generated-shown?) (assoc-in [:namespaces "orchard.generated"] (generated-node))))

(defn- source-files
  "A page's files for `source-key`. The bundle holds only working-tree
  text, so other snapshots come back empty."
  [bundle namespace-id source-key]
  (let [working-files (or (:files (get @made-files namespace-id))
                          (when (= "orchard.generated" namespace-id) [{:path "target/generated.clj" :text generated-text}])
                          (get-in bundle [:sources namespace-id] []))]
    (if (= source-key (ui-state/source-key (working-snapshot bundle) namespace-id))
      working-files
      (mapv #(assoc % :text "") working-files))))

(defmulti ^:private answer!
  "Replies to one view message from the loaded bundle, the way the
  extension would."
  (fn [_bundle message _deliver!] (:type message)))

(defmethod answer! :default [_bundle _message _deliver!]
  nil)

(def ^:private present-agents
  "The agents the harness acts out as connected."
  (atom []))

(def ^:private reporting-level (atom :full))

(def ^:private wavescope-bands (atom :fine))

(defn- agents-message []
  {:type :host/agents
   :serving? (not= "off" (query-parameter "agents"))
   :url "http://127.0.0.1:41234/mcp"
   :agents @present-agents
   :reporting @reporting-level
   :wavescope-bands @wavescope-bands
   :problem nil})

(def ^:private acted-presence
  "Where the harness's pretend agents work."
  (atom {:agents [] :pins []}))

(defn- demo-presence
  "Two agents at work in the orchard as of `now`, Claude Code reading
  core.clj with a pin on it, and opencode having changed clamp in
  util.clj, a change credited by CodeCloud's guess."
  [now]
  (let [seconds 1000]
    {:agents [{:id "session-1" :name "Claude Code" :client "claude-code" :initials "CC" :hue 235
               :since (- now (* 600 seconds)) :last-at (- now (* 5 seconds)) :present? true
               :place {:path "src/orchard/core.clj" :line 5} :doing "reading core.clj around line 5" :intent nil
               :reads [{:path "src/orchard/core.clj" :start 1 :end 12 :at (- now (* 5 seconds))}]
               :changes []}
              {:id "session-2" :name "opencode" :client "opencode" :initials "Op" :hue 150
               :since (- now (* 900 seconds)) :last-at (- now (* 20 seconds)) :present? true
               :place {:path "src/orchard/util.clj" :line 4} :doing "changing util.clj" :intent "tidying clamp"
               :reads [{:path "src/orchard/util.clj" :start 1 :end 26 :at (- now (* 90 seconds))}]
               :changes [{:path "src/orchard/util.clj" :start 4 :end 5 :added 2 :removed 1 :at (- now (* 20 seconds))
                          :credited? true}]}]
     :pins [{:id "pin-1" :agent-id "session-1" :path "src/orchard/core.clj" :line 9 :note "grow repeats plant"
             :at (- now (* 8 seconds))}]}))

(defn- presence-message []
  (assoc @acted-presence :type :host/presence))

;; An assistant stand-in, acting out one answer the way the extension
;; streams a real one: a word of opening, two tools, then the answer.

(def ^:private stand-in-model "stand-in from the harness")

(def ^:private acted-assistant
  "The conversation the harness acts out, with the timers of the answer
  under way."
  (atom {:entries [] :next-id 1 :running? false :model stand-in-model :ready? true :hub? true :timers []}))

(defn- assistant-message []
  (-> (select-keys @acted-assistant [:entries :running? :model :ready? :hub?])
      (assoc :type :host/assistant)))

(defn- with-entry
  "The acted conversation with `entry` in place of the entry holding its
  id, its version counted up, or added after the rest."
  [acted {:keys [id] :as entry}]
  (update acted :entries
          (fn [entries]
            (if-let [found (some #(when (= id (:id %)) %) entries)]
              (mapv #(if (= id (:id %)) (assoc entry :version (inc (:version found))) %) entries)
              (conj entries (assoc entry :version 1))))))

(defn- with-notice [acted text]
  (-> acted
      (with-entry {:id (:next-id acted) :kind :notice :text text})
      (update :next-id inc)))

(def ^:private scripted-answer
  (str "**orchard.core** grows trees in four steps that share one shape.\n\n"
       "- `plant` at src/orchard/core.clj:6 reads the soil's moisture, normalizes it, and prunes.\n"
       "- `grow`, `water`, and `tend` repeat that line, so a change to one belongs in all four.\n\n"
       "Each step leans on `util/normalize`, at src/orchard/util.clj:7:\n\n"
       "```clojure\n(trees/prune (util/normalize (soil/moisture x)))\n```"))

(defn- answer-steps
  "The steps of the acted answer to `question`, each [pause change], its
  entries numbered from `first-id`. With `tools?` the answer opens with
  a word and two tools; without them it answers at once."
  [question first-id tools?]
  (let [[question-id opening-id overview-id read-id answer-id] (range first-id (+ first-id 5))
        opening "Let me look at the cloud first."
        overview {:id overview-id :kind :tool :name "cloud_overview" :title "The codebase at a glance" :target nil}
        reading {:id read-id :kind :tool :name "read_file" :title "Read a file" :target "src/orchard/core.clj:6"}
        streamed (reductions str (re-seq #"\S+\s*" scripted-answer))
        ;; Milliseconds between the streamed pieces of the answer.
        word-spacing 45]
    (concat
     [[0 #(-> % (with-entry {:id question-id :kind :question :text question}) (assoc :running? true :next-id (+ first-id 5)))]]
     (when tools?
       [[150 #(with-entry % {:id opening-id :kind :answer :text "" :streaming? true})]
        [200 #(with-entry % {:id opening-id :kind :answer :text opening :streaming? true})]
        [120 #(with-entry % {:id opening-id :kind :answer :text opening :streaming? false})]
        [80 #(with-entry % (assoc overview :status :running))]
        [400 #(with-entry % (assoc overview :status :done :result "orchard: 17 pages in 17 files\nsrc/orchard: core, util, soil, trees, and nine more"))]
        [80 #(with-entry % (assoc reading :status :running))]
        [350 #(with-entry % (assoc reading :status :done
                                   :result "(defn plant [x]\n  (trees/prune (util/normalize (soil/moisture x))))"))]])
     [[120 #(with-entry % {:id answer-id :kind :answer :text "" :streaming? true})]]
     (for [text streamed]
       [word-spacing #(with-entry % {:id answer-id :kind :answer :text text :streaming? true})])
     [[60 #(-> % (with-entry {:id answer-id :kind :answer :text scripted-answer :streaming? false}) (assoc :running? false :timers []))]])))

(defn- act-out!
  "Plays `steps`, each [pause change], handing the view the conversation
  after each."
  [steps deliver!]
  (let [timers (second (reduce (fn [[at timers] [pause change]]
                                 (let [step-at (+ at pause)]
                                   [step-at (conj timers (js/setTimeout (fn []
                                                                          (swap! acted-assistant change)
                                                                          (deliver! (assistant-message)))
                                                                        step-at))]))
                               [0 []]
                               steps))]
    (swap! acted-assistant assoc :timers timers)))

(defn- halted
  "The acted conversation with its answer stopped where it was, nothing
  streaming and a tool still running failed."
  [acted]
  (run! js/clearTimeout (:timers acted))
  (-> acted
      (update :entries (fn [entries]
                         (mapv (fn [{:keys [streaming? status version] :as entry}]
                                 (cond
                                   streaming? (assoc entry :streaming? false :version (inc version))
                                   (= :running status) (assoc entry :status :failed :version (inc version)
                                                              :result "The person stopped the question before this answered.")
                                   :else entry))
                               entries)))
      (assoc :running? false :timers [])))

(defmethod answer! :view/assistant-ask [_bundle {:keys [text]} deliver!]
  (let [{:keys [running? ready? next-id]} @acted-assistant]
    (cond
      running? nil
      (not ready?) (do (swap! acted-assistant with-notice "Choose a model for the assistant first.")
                       (deliver! (assistant-message)))
      :else (act-out! (answer-steps text next-id (:hub? @acted-assistant)) deliver!))))

(defmethod answer! :view/assistant-stop [_bundle _message deliver!]
  (when (:running? @acted-assistant)
    (swap! acted-assistant #(with-notice (halted %) "Stopped."))
    (deliver! (assistant-message))))

(defmethod answer! :view/assistant-new [_bundle _message deliver!]
  (swap! acted-assistant #(assoc (halted %) :entries []))
  (deliver! (assistant-message)))

(defmethod answer! :view/assistant-model [_bundle _message deliver!]
  (show-toast! "CodeCloud: Choose the Assistant's Model opens here; the harness picks its stand-in.")
  (swap! acted-assistant assoc :ready? true :model stand-in-model)
  (later! deliver! 300 (assistant-message)))

(defmethod answer! :view/dismiss-pin [_bundle {:keys [pin-id]} deliver!]
  (swap! acted-presence update :pins (fn [pins] (filterv #(not= pin-id (:id %)) pins)))
  (later! deliver! 60 (presence-message)))

(defmethod answer! :view/ready [bundle _message deliver!]
  (deliver! (init-message bundle))
  (reset! present-agents (into []
                               (comp (remove #(contains? #{"" "off"} %))
                                     (map (fn [agent-name] {:name agent-name :since (js/Date.now)})))
                               (string/split (or (query-parameter "agents") "") #",")))
  (deliver! (agents-message))
  (swap! acted-assistant assoc
         :ready? (not= "none" (query-parameter "assistant"))
         :model (when (not= "none" (query-parameter "assistant")) stand-in-model)
         :hub? (not= "off" (query-parameter "agents")))
  (deliver! (assistant-message))
  (when (= "demo" (query-parameter "presence"))
    (reset! acted-presence (demo-presence (js/Date.now)))
    (later! deliver! 700 (presence-message)))
  (later! deliver! 40 {:type :host/editor-colors :code-colors (code-colors/theme-colors sample-theme)})
  (deliver! {:type :host/views :views (:views bundle {}) :color-layers (:color-layers bundle [])})
  (deliver! {:type :host/status :message "Reading the codebase" :progress 0.6})
  (later! deliver! 150 {:type :host/history :commits (:commits bundle) :available? (boolean (seq (:commits bundle)))})
  (later! deliver! 200 {:type :host/snapshot :snapshot (working-snapshot bundle)})
  (later! deliver! 260 {:type :host/status :message nil})
  (when-let [arrival (query-parameter "arrive")]
    (let [[path line] (string/split arrival #":")
          arrival-delay (or (some-> (query-parameter "arrive-after") (js/parseInt 10)) 1600)]
      (later! deliver! arrival-delay {:type :host/arrive :path path :line (js/parseInt line 10) :text-scale 0.67}))))

(defmethod answer! :view/select-snapshot [bundle {:keys [snapshot-id]} deliver!]
  (later! deliver! 120 {:type :host/snapshot
                        :snapshot (if (= :working-tree snapshot-id)
                                    (working-snapshot bundle)
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

(defn- bundle-text [bundle path]
  (some #(when (= path (:path %)) (:text %)) (apply concat (vals (:sources bundle)))))

(defn- bracket-problems [text]
  (for [{[line column] :start :keys [closed? kind]} (forms-state/read-forms (editing-state/text-lines text))
        :when (and (not closed?) (not= :atom kind))]
    {:line line :column column :end-line line :end-column (inc column) :severity :error
     :message "This bracket is never closed." :source "harness" :code "unclosed-bracket"}))

(defonce ^:private reported-problems (atom {}))

(defn- report-problems! [deliver! path text]
  (let [problems (when (string/ends-with? path ".clj") (vec (bracket-problems text)))]
    (swap! reported-problems #(if (seq problems) (assoc % path problems) (dissoc % path)))
    (later! deliver! 260 {:type :host/diagnostics :files @reported-problems})))

(defmethod answer! :view/edit [bundle {:keys [path revision text] [line column end-line end-column] :range} deliver!]
  (let [lines (string/split (or (get @edited-texts path) (bundle-text bundle path) (some #(when (= path (:path %)) (:text %))
                                                                                        (mapcat :files (vals @made-files)))
                                "")
                            #"\n" -1)
        offset (fn [target-line target-column]
                 (+ target-column (reduce + 0 (map #(inc (count %)) (take target-line lines)))))
        joined (string/join "\n" lines)
        updated (str (subs joined 0 (offset line column)) text (subs joined (offset end-line end-column)))]
    (swap! edited-texts assoc path updated)
    (later! deliver! 30 {:type :host/edited :path path :revision revision :content-key (str "edited:" revision) :dirty? true})
    (report-problems! deliver! path updated)))

(defmethod answer! :view/format [_bundle {:keys [path language revision epoch text start-line end-line]} deliver!]
  (let [lines (editing-state/text-lines text)
        [first-line last-line] (if start-line [start-line end-line] [0 (dec (count lines))])]
    (later! deliver! 40 {:type :host/formatted :path path :revision revision :epoch epoch
                         :text (if (= :clojure language)
                                 (string/join "\n" (clojure-format/reindent lines first-line first-line last-line))
                                 text)})))

(defn- made-node
  "A page for a Clojure file just made at `path` holding `text`, as the
  analyzer would read it."
  [path namespace-name text]
  (let [line-count (count (editing-state/text-lines text))]
    {:definitions [] :parse-error? false :name namespace-name :cycle-id nil :external-requires [] :hotspots []
     :requires [] :id namespace-name :kind :namespace :required-by []
     :files [{:path path :dialect (last (string/split path #"\.")) :line-count line-count
              :longest-line (apply max 0 (map count (editing-state/text-lines text))) :content-key (str "made:" path)
              :parse-error nil}]
     :doc nil :flag-counts {}
     :metrics {:definitions 0 :dependents 0 :functions 0 :lines line-count :references 0 :complection 0
               :external-dependencies 0 :flags 0 :dependencies 0}
     :importance 0.1}))

(defmethod answer! :view/new-file [bundle {:keys [directory]} deliver!]
  (let [prefix (if (string/blank? directory) "" (str directory "/"))
        typed (or (some-> js/window .-codecloudNewFileName) (js/prompt "The new file's path" prefix))
        problem (when typed (new-file/name-problem typed))]
    (cond
      (nil? typed) nil
      problem (show-toast! problem)
      :else
      (let [path (string/trim typed)
            snapshot (working-snapshot bundle)
            [text line column] (new-file/starting-text snapshot path)
            namespace-name (new-file/namespace-name snapshot path)]
        (if (string/blank? text)
          (show-toast! (str "The extension would open " path " in an editor."))
          (do (swap! made-files assoc namespace-name {:node (made-node path namespace-name text)
                                                      :files [{:path path :text text}]})
              (later! deliver! 30 {:type :host/created :path path :line line :column column})
              (later! deliver! 400 {:type :host/snapshot :snapshot (working-snapshot bundle)})))))))

(defmethod answer! :view/save [_bundle {:keys [path]} deliver!]
  (show-toast! (str "The extension would save " path "."))
  (later! deliver! 60 {:type :host/dirty :path path :dirty? false}))

(defmethod answer! :view/save-view [_bundle message _deliver!]
  (show-toast! (str "The extension would save the view " (pr-str (:name message)) " in the cloud file.")))

(defmethod answer! :view/save-preference [bundle {preference :key :keys [value]} deliver!]
  (try (.setItem js/localStorage (str "codecloud:" (name preference)) (pr-str value))
       (catch :default _error nil))
  (when (= :shown-ignored preference)
    (later! deliver! 300 {:type :host/snapshot :snapshot (working-snapshot bundle)})))

(defmethod answer! :view/list-ignored [_bundle {folder :path} deliver!]
  (later! deliver! 80 {:type :host/ignored :path folder :entries (get ignored-stand-in folder []) :more 0}))

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
        if (let [[condition then otherwise] arguments]
             (if (harness-eval locals condition) (harness-eval locals then) (harness-eval locals otherwise)))
        do (last (map #(harness-eval locals %) arguments))
        (let [function (harness-eval locals head)]
          (when-not (ifn? function)
            (throw (ex-info (str (pr-str function) " cannot be called") {})))
          (apply function (map #(harness-eval locals %) arguments)))))
    :else form))

(defn- harness-replies
  "The replies an nREPL server would send evaluating `code` for request
  `id` in namespace `ns`, with printed output and a value for each form,
  or an error stopping at the form that failed, then done."
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

(defn- deliver-replies! [deliver! tab replies]
  (doseq [[index reply] (map-indexed vector replies)]
    (later! deliver! (+ 80 (* 30 index)) (assoc reply :type :host/repl-output :tab tab))))

(defmethod answer! :view/repl-connect [_bundle {:keys [tab port via]} deliver!]
  (let [shown-port (or port 7888)]
    (cond
      (= :calva via)
      (later! deliver! 300 {:type :host/repl-status :tab tab :status :connected :via :calva
                            :message "Calva's REPL, session clj"})

      (and (nil? port) (= "missing" (query-parameter "repl")))
      (later! deliver! 120 {:type :host/repl-status :tab tab :status :failed
                            :message "No nREPL port file in this folder or a folder above it. Start an nREPL server there, or type its port and press Enter."})

      :else
      (do (later! deliver! 100 {:type :host/repl-status :tab tab :status :connecting :port shown-port
                                :message (str "Connecting to port " shown-port "…")})
          (later! deliver! 450 {:type :host/repl-status :tab tab :status :connected :port shown-port
                                :message (str "Harness REPL on port " shown-port)})))))

(defmethod answer! :view/focus [_bundle message _deliver!]
  ;; The latest report stays on the window, where a test can read it.
  (set! (.-codecloudFocus js/window) (pr-str (dissoc message :protocol/version))))

(defmethod answer! :view/connect-agent [_bundle _message deliver!]
  (show-toast! "CodeCloud: Connect an Agent opens here; the harness connects a pretend Claude Code.")
  (swap! present-agents conj {:name (if (seq @present-agents) (str "Claude Code " (inc (count @present-agents))) "Claude Code")
                              :since (js/Date.now)})
  (later! deliver! 400 (agents-message)))

(defmethod answer! :view/agents-reporting [_bundle {:keys [level]} deliver!]
  (reset! reporting-level level)
  (later! deliver! 200 (agents-message)))

(defmethod answer! :view/agents-wavescope-bands [_bundle {:keys [bands]} deliver!]
  (reset! wavescope-bands bands)
  (later! deliver! 200 (agents-message)))

(defmethod answer! :view/repl-disconnect [_bundle {:keys [tab]} deliver!]
  (later! deliver! 60 {:type :host/repl-status :tab tab :status :disconnected :message "Disconnected."}))

(defmethod answer! :view/repl-eval [_bundle {:keys [tab id code ns]} deliver!]
  (deliver-replies! deliver! tab (harness-replies id code ns)))

(defmethod answer! :view/repl-load-file [_bundle {:keys [tab id code]} deliver!]
  (let [replies (harness-replies id code nil)
        answered (butlast replies)]
    ;; Loading a file answers with the value of its last form alone.
    (deliver-replies! deliver! tab (concat (remove :value answered) (take-last 1 (filter :value answered)) [(last replies)]))))

(defmethod answer! :view/repl-load-files [bundle {:keys [tab id paths]} deliver!]
  (let [texts (into {} (for [files (vals (:sources bundle))
                             {:keys [path text]} files]
                         [path text]))
        loaded (for [path paths
                     :let [text (get texts path)]
                     :when text]
                 (do (harness-replies id text nil) path))]
    (deliver-replies! deliver! tab (concat (for [path loaded] {:id id :note (str "Loaded " path)})
                                           [{:id id :value (str "Loaded " (count loaded) " files")}
                                            {:id id :status ["done"]}]))))

;; A shell stand-in, answering commands the way a terminal's output
;; arrives, without running anything.

(defmethod answer! :view/shell-open [_bundle {:keys [tab terminal]} deliver!]
  (later! deliver! 150 {:type :host/shell-status :tab tab :status :ready :label (if terminal "zsh" "bash")
                        :cwd "/home/harness/orchard" :message (str "Harness shell " (if terminal "attached" "opened") ".")}))

(defonce ^:private reading-command (atom nil))

(defmethod answer! :view/shell-run [_bundle {:keys [tab id command]} deliver!]
  (if (string/starts-with? command "read")
    ;; A command reading a line asks for it and waits for the tab to send one.
    (do (reset! reading-command id)
        (later! deliver! 120 {:type :host/shell-output :tab tab :id id :text "name? "}))
    (do (later! deliver! 120 {:type :host/shell-output :tab tab :id id
                              :text (str "harness would run: " command "\n")})
        (later! deliver! 240 {:type :host/shell-output :tab tab :id id :done? true :exit-code (if (= "false" command) 1 0)
                              :cwd "/home/harness/orchard"}))))

(defmethod answer! :view/shell-input [_bundle {:keys [tab text]} deliver!]
  (when-let [id @reading-command]
    (reset! reading-command nil)
    ;; A terminal echoes the line typed, then the command answers it.
    (later! deliver! 60 {:type :host/shell-output :tab tab :id id :text (str text "\nhello " text "\n")})
    (later! deliver! 160 {:type :host/shell-output :tab tab :id id :done? true :exit-code 0 :cwd "/home/harness/orchard"})))

(def ^:private window-files
  "Where the harness serves the files its windows show, relative to the
  harness page, standing in for the cloud's folder."
  "../test-resources/windows/")

(defonce ^:private window-versions (atom {}))

(defn- deliver-window! [deliver! window-id fields]
  (let [version (get (swap! window-versions update window-id (fnil inc 0)) window-id)]
    (later! deliver! 30 (assoc fields :type :host/window :id window-id :version version))))

(defmethod answer! :view/window-open [_bundle {:keys [id kind source]} deliver!]
  (let [address (str window-files source)]
    (case kind
      :web (deliver-window! deliver! id {:url source})
      :markdown (-> (js/fetch address #js {:cache "no-store"})
                    (.then (fn [^js response]
                             (if (.-ok response)
                               (.then (.text response)
                                      (fn [text]
                                        (deliver-window! deliver! id {:text text
                                                                      :base-url (string/replace address #"/[^/]*$" "")})))
                               (deliver-window! deliver! id {:error (str "No file at " source)})))))
      (deliver-window! deliver! id {:url address}))))

(defmethod answer! :view/window-close [_bundle _message _deliver!]
  nil)

(defmethod answer! :view/window-pick-file [_bundle _message deliver!]
  (show-toast! "The extension would ask for a file; the harness opens guide.md.")
  (later! deliver! 60 {:type :host/open-window :kind :markdown :source "guide.md"}))

(defmethod answer! :view/window-reveal [_bundle {:keys [source]} _deliver!]
  (show-toast! (str "The extension would open " source ".")))

(defmethod answer! :view/run-page-command [_bundle {:keys [title command shell path line column]} _deliver!]
  (show-toast! (str "The extension would run " title ", " (or command shell) ", for " path " at " line ":" column ".")))

(defmethod answer! :view/pick-page-command [_bundle {:keys [path line]} _deliver!]
  (show-toast! (str "The extension would offer every command to run for " path " at line " line ".")))

(defmethod answer! :view/list-terminals [_bundle _message deliver!]
  (later! deliver! 60 {:type :host/terminals :terminals [{:id "terminal-1" :name "zsh"}] :calva? true}))

(defn- harness-transport []
  (let [handlers (atom [])
        deliver! (fn [reply] (doseq [handler @handlers] (handler reply)))
        bundle (load-bundle)]
    {:subscribe! (fn [handler] (swap! handlers conj handler))
     :send! (fn [message] (.then bundle (fn [loaded] (answer! loaded message deliver!))))
     :deliver! deliver!
     :view-state nil}))

(defn init! []
  (let [transport (harness-transport)
        started (app/start! (js/document.getElementById "codecloud") transport)
        visitor-delay 1500]
    (set! (.-codecloudApp js/window) started)
    (set! (.-codecloudDispatch js/window) (fn [event-text] ((:dispatch! started) (reader/read-string event-text))))
    (set! (.-codecloudDeliver js/window) (fn [message-text]
                                            (let [message (reader/read-string message-text)]
                                              ;; A script's presence becomes the harness's own, as a host's would.
                                              (when (= :host/presence (:type message))
                                                (reset! acted-presence (select-keys message [:agents :pins])))
                                              ((:deliver! transport) message))))
    (set! (.-codecloudScreenOf js/window)
          (fn [namespace-id]
            (when-let [{:keys [screen-x screen-y half-width-pixels half-height-pixels]}
                       (pages/page-of (:page-layer started) namespace-id)]
              #js [screen-x screen-y half-width-pixels half-height-pixels])))
    (set! (.-codecloudPointOf js/window)
          (fn [namespace-id path line visual-column]
            (when-let [page-object (pages/page-of (:page-layer started) namespace-id)]
              (some-> (editing-view/line-screen-point page-object path line visual-column) clj->js))))
    (set! (.-codecloudFlying js/window) (fn [] (navigation/flying? (:controller started))))
    (set! (.-codecloudStateAt js/window)
          (fn [path-text] (pr-str (get-in @(:state started) (reader/read-string path-text)))))
    (set! (.-codecloudTurn js/window)
          (fn [azimuth polar]
            (let [^js spherical (:spherical (:controller started))]
              (set! (.-theta spherical) azimuth)
              (set! (.-phi spherical) polar))))
    (when-let [visitor (query-parameter "visitor")]
      (js/setTimeout #(backdrop/send-for! (:backdrop started) (keyword visitor)) visitor-delay))))

(init!)
