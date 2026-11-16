(ns codecloud.render.hud.assistant-view
  "The assistant pane at the right of the sky, with the model chosen, the
  conversation, and a box to ask in. Answers read as Markdown, and the
  places they name fly there when clicked, as does the file a tool read.
  Enter asks, Shift+Enter starts a new line, and Escape leaves the box."
  (:require [clojure.string :as string]
            [codecloud.presentation.assistant-state :as assistant-state]
            [codecloud.presentation.markdown :as markdown]
            [codecloud.presentation.presence-state :as presence-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.dock :as dock]
            [codecloud.render.icons :as icons]))

;; Entries

(defn- place-button [dispatch {:keys [path line] :as place} children]
  (into [:button.assistant-place {:type "button"
                                  :title (str "Fly to " path (when line (str ", line " line)))
                                  :on-click #(dispatch [:show-place place])}]
        children))

(defn- toggle-result!
  "Shows or hides the answer of the tool whose disclosure was clicked."
  [^js event]
  (let [^js button (.-currentTarget event)
        ^js tool (.closest button ".assistant-tool")
        expanded? (not (.contains (.-classList tool) "is-expanded"))]
    (dom/toggle-class! tool "is-expanded" expanded?)
    (dom/toggle-class! button "is-open" expanded?)
    (.setAttribute button "aria-expanded" (str expanded?))))

(def ^:private status-words
  {:running "running" :done "done" :failed "failed"})

(defn- tool-hiccup
  "A tool the assistant called, with its title, what it was about, whether
  it runs, finished, or failed, and its answer behind a disclosure."
  [dispatch place-for {:keys [title target status result]}]
  (let [place (when target (place-for target))]
    [:div.assistant-tool {:class (str "is-" (name (or status :running)))}
     [:div.assistant-tool-row
      [:span.assistant-tool-mark {:aria-hidden "true"}]
      [:span.assistant-tool-title title [:span.visually-hidden (str ", " (get status-words status ""))]]
      (cond
        place (place-button dispatch place [target])
        target [:span.assistant-tool-target target]
        :else nil)
      (when result
        [:button.assistant-tool-toggle {:type "button" :aria-expanded "false"
                                        :title "What the tool answered" :aria-label "What the tool answered"
                                        :on-click toggle-result!}
         icons/disclosure])]
     (when result [:pre.assistant-tool-result result])]))

(def ^:private waiting-mark
  [:span.assistant-waiting {:role "status" :aria-label "Waiting for the reply"} [:span] [:span] [:span]])

(defn- entry-element [dispatch place-for {:keys [kind text streaming?] :as entry}]
  (dom/element
   (case kind
     :question [:div.assistant-question text]
     :answer (if (and streaming? (string/blank? text))
               [:div.assistant-answer.is-streaming waiting-mark]
               (into [:div.assistant-answer {:class (when streaming? "is-streaming")}]
                     (markdown/hiccup text {:place-for place-for
                                            :place-link #(place-button dispatch %1 %2)})))
     :tool (tool-hiccup dispatch place-for entry)
     [:p.assistant-notice text])))

(defn- sync-transcript!
  "Brings the transcript's elements in line with `entries`, redrawing only
  entries whose version changed, and follows the newest when scrolled to
  the end or `follow?`."
  [^js transcript rendered entries draw follow?]
  (let [end-slack 32
        following? (or follow?
                       (< (- (.-scrollHeight transcript) (.-scrollTop transcript) (.-clientHeight transcript)) end-slack))
        drawn (into {} (map (juxt :id identity)) @rendered)
        kept-ids (into #{} (map :id) entries)
        current (mapv (fn [{:keys [id version] :as entry}]
                        (let [{drawn-version :version :keys [^js node]} (get drawn id)]
                          (cond
                            (and node (= version drawn-version)) {:id id :version version :node node}
                            node (let [replacement (draw entry)]
                                   (.replaceWith node replacement)
                                   {:id id :version version :node replacement})
                            :else {:id id :version version :node (draw entry)})))
                      entries)]
    (doseq [{:keys [id ^js node]} @rendered
            :when (not (contains? kept-ids id))]
      (.remove node))
    (reduce (fn [^js expected {:keys [^js node]}]
              (when-not (identical? expected node)
                (.insertBefore transcript node expected))
              (.-nextSibling node))
            (.-firstChild transcript)
            current)
    (reset! rendered current)
    (when following?
      (set! (.-scrollTop transcript) (.-scrollHeight transcript)))))

(defn- finder-for
  "The place finder for the files of `snapshot`, kept while the snapshot,
  or at least its paths, stay the same, so the conversation is not drawn
  again for nothing."
  [cache snapshot]
  (let [[cached-snapshot cached-paths finder] @cache]
    (if (identical? snapshot cached-snapshot)
      finder
      (let [paths (set (keys (presence-state/page-index snapshot)))
            kept (if (= paths cached-paths) finder (assistant-state/place-finder paths))]
        (reset! cache [snapshot paths kept])
        kept))))

;; Asking

(defn- grow-input! [^js input]
  (let [tallest-pixels 132]
    (dom/fit-height! input tallest-pixels)))

(defn- show-send-button!
  "Lets the person ask once something is typed and no question runs."
  [^js root ^js input {:keys [running?]}]
  (set! (.-disabled (dom/find-in root ".assistant-send")) (boolean (or running? (string/blank? (.-value input))))))

(defn- send!
  "Asks what was typed, emptying the box when a model can take it. With
  no model chosen the typed question stays while the picker opens."
  [dispatch ^js root ^js input shown]
  (let [text (string/trim (.-value input))
        {:keys [ready? running?]} @shown]
    (when (and (seq text) (not running?))
      (dispatch [:assistant-ask text])
      (when ready?
        (set! (.-value input) "")
        (grow-input! input)
        (show-send-button! root input (assoc @shown :running? true))))))

(defn- on-key-down!
  "Answers Enter and Escape typed into the box, keeping them from the
  cloud and VS Code, whose keybindings get the rest."
  [dispatch ^js root ^js input shown ^js event]
  (let [typed-key (.-key event)
        answered! (fn []
                    (.preventDefault event)
                    (.stopPropagation event))]
    (cond
      (and (= "Enter" typed-key) (not (.-shiftKey event)) (not (.-isComposing event)))
      (do (answered!)
          (send! dispatch root input shown))

      (= "Escape" typed-key)
      (do (answered!)
          (.blur input)))))

(def panel
  {:id :assistant
   :mount (fn [dispatch]
            (let [^js root (dom/element
                            [:aside.hud-assistant {:aria-label "Assistant"}
                             [:header.panel-header.assistant-header
                              [:h2 "Assistant"]
                              [:button.icon-button.assistant-new {:type "button" :title "New conversation"
                                                                  :aria-label "New conversation"
                                                                  :on-click #(dispatch [:assistant-new])}
                               icons/new-conversation]
                              (dock/minimize-button dispatch :assistant)
                              [:button.icon-button.assistant-close {:type "button" :title "Close (Q)"
                                                                    :aria-label "Close the assistant"
                                                                    :on-click #(dispatch [:toggle-assistant])}
                               icons/close]]
                             [:button.assistant-model {:type "button" :title "Choose the assistant's model"
                                                       :on-click #(dispatch [:assistant-model])}
                              icons/assistant
                              [:span.assistant-model-name]
                              [:span.assistant-model-change "Change"]]
                             [:p.assistant-hub-note
                              "The agent hub is off, so the assistant answers without CodeCloud's tools. "
                              "Turn on codecloud.agents.enabled to give it them."]
                             [:div.assistant-empty
                              [:div.assistant-choose
                               [:p "Once a model is chosen, the assistant answers questions about this codebase "
                                "from what CodeCloud's tools find."]
                               [:button.action-button {:type "button" :on-click #(dispatch [:assistant-model])}
                                icons/assistant "Choose a model"]]
                              [:p.assistant-hint
                               "Ask about this codebase. The places an answer names fly there when clicked."]]
                             [:div.assistant-transcript {:aria-label "Conversation"}]
                             [:div.assistant-compose
                              [:textarea.assistant-input {:rows "1" :placeholder "Ask about this codebase"
                                                          :aria-label "Question" :spellcheck "true"}]
                              [:button.icon-button.assistant-send {:type "button" :title "Ask (Enter)" :aria-label "Ask"
                                                                   :disabled true}
                               icons/send]
                              [:button.icon-button.assistant-stop {:type "button" :title "Stop" :aria-label "Stop"
                                                                   :on-click #(dispatch [:assistant-stop])}
                               icons/interrupt]]])
                  ^js input (dom/find-in root ".assistant-input")
                  shown (atom assistant-state/initial)]
              (.addEventListener input "keydown" #(on-key-down! dispatch root input shown %))
              (.addEventListener input "input" (fn [_]
                                                 (grow-input! input)
                                                 (show-send-button! root input @shown)))
              (.addEventListener (dom/find-in root ".assistant-send") "click"
                                 (fn [_]
                                   (send! dispatch root input shown)
                                   (.focus input)))
              {:root root
               :input input
               :transcript (dom/find-in root ".assistant-transcript")
               :dispatch dispatch
               :shown shown
               :rendered (atom [])
               :synced (atom nil)
               :drawn-with (atom nil)
               :finders (atom [::no-snapshot-yet nil nil])
               :was-open? (atom false)}))
   :update (fn [{:keys [^js root ^js input ^js transcript dispatch shown rendered synced drawn-with finders was-open?]}
                {:keys [assistant editing snapshot]}]
             (let [{:keys [open? entries running? model ready? hub?]} assistant
                   opening? (and open? (not @was-open?))]
               (reset! shown assistant)
               (dom/toggle-class! root "is-open" open?)
               (when open?
                 (let [place-for (finder-for finders snapshot)
                       asked? (> (count (filter #(= :question (:kind %)) entries))
                                 (count (filter #(= :question (:kind %)) @synced)))]
                   (dom/set-text! (dom/find-in root ".assistant-model-name") (or model "Choose a model"))
                   (dom/toggle-class! (dom/find-in root ".assistant-model-change") "is-hidden" (not ready?))
                   (dom/toggle-class! (dom/find-in root ".assistant-hub-note") "is-hidden" hub?)
                   (dom/toggle-class! (dom/find-in root ".assistant-empty") "is-hidden" (seq entries))
                   (dom/toggle-class! (dom/find-in root ".assistant-choose") "is-hidden" ready?)
                   (dom/toggle-class! (dom/find-in root ".assistant-hint") "is-hidden" (not ready?))
                   (dom/toggle-class! root "is-running" running?)
                   (set! (.-disabled (dom/find-in root ".assistant-new")) (and (empty? entries) (not running?)))
                   (show-send-button! root input assistant)
                   (when-not (identical? place-for @drawn-with)
                     (reset! drawn-with place-for)
                     (reset! rendered [])
                     (reset! synced nil)
                     (set! (.-textContent transcript) ""))
                   (when (or opening? (not (identical? entries @synced)))
                     (sync-transcript! transcript rendered entries #(entry-element dispatch place-for %) (or opening? asked?))
                     (reset! synced entries))))
               (when (and opening? (not editing))
                 (js/requestAnimationFrame #(.focus input #js {:preventScroll true})))
               (when (and (not open?) @was-open? (identical? input (.-activeElement js/document)))
                 (.blur input))
               (reset! was-open? open?)))})
