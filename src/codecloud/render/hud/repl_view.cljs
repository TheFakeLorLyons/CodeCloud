(ns codecloud.render.hud.repl-view
  "The REPL pane, a window over the sky holding tabs, each a REPL or a
  shell, with the shown tab's log and a line to type into. Its header
  moves it and its edges and corners resize it, and both are remembered.
  Enter sends what was typed, in a REPL tab once its forms are closed,
  the arrow keys walk back through what was sent, and Ctrl+C with nothing
  selected interrupts."
  (:require [clojure.string :as string]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.forms-state :as forms-state]
            [codecloud.presentation.repl-state :as repl-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

;; The log

(defn- linked
  "`text` as strings and links, each web address in it a link that shows
  its page in a window, its last character never closing punctuation."
  [text]
  (let [address-pattern #"https?://[^\s\"'<>]*[^\s\"'<>.,;:!?)\]]"]
    (loop [remaining text
           parts []]
      (if-let [address (re-find address-pattern remaining)]
        (let [start (string/index-of remaining address)]
          (recur (subs remaining (+ start (count address)))
                 (conj parts (subs remaining 0 start) [:a.repl-link {:href "#" :title "Show it in a window"} address])))
        (conj parts remaining)))))

(defn- entry-element
  "An element showing one log entry, without the line break most output
  ends with, its web addresses links."
  [{:keys [kind text]}]
  (dom/element (into [:div {:class ["repl-line" (str "is-" (name kind))]}]
                     (linked (string/replace text #"\n$" "")))))

(defn- shown-entry
  "Appends an element for `entry` to the log, returning what the pane
  keeps of it."
  [^js log-element entry]
  (let [node (entry-element entry)]
    (.appendChild log-element node)
    {:number (:number entry) :text (:text entry) :node node}))

(defn- sync-log!
  "Brings the log's elements in line with the log, growing the last entry
  and adding new ones, so a stream of output costs little. Follows the
  newest when scrolled to the end."
  [^js log-element rendered log]
  (let [end-slack 24
        following? (< (- (.-scrollHeight log-element) (.-scrollTop log-element) (.-clientHeight log-element)) end-slack)
        first-number (:number (first log))
        last-number (:number (peek log))
        [dropped kept] (split-with #(< (:number %) (or first-number js/Infinity)) @rendered)]
    (if (or (empty? log) (some #(> (:number %) last-number) kept))
      (do (set! (.-textContent log-element) "")
          (reset! rendered (mapv #(shown-entry log-element %) log)))
      (let [_ (doseq [{:keys [^js node]} dropped] (.remove node))
            grown (if-let [{:keys [number text ^js node]} (last kept)]
                    (let [current (nth log (- number first-number))]
                      (if (= text (:text current))
                        (vec kept)
                        (let [replacement (entry-element current)]
                          (.replaceWith node replacement)
                          (conj (vec (butlast kept)) {:number number :text (:text current) :node replacement}))))
                    [])
            newest-shown (if (seq grown) (:number (peek grown)) (dec first-number))]
        (reset! rendered (into grown
                               (comp (filter #(> (:number %) newest-shown))
                                     (map #(shown-entry log-element %)))
                               log))))
    (when following?
      (set! (.-scrollTop log-element) (.-scrollHeight log-element)))))

;; Typing

(defn- grow-input! [^js input]
  (let [tallest-pixels 96]
    (dom/fit-height! input tallest-pixels)))

(defn- set-input! [^js input text]
  (set! (.-value input) text)
  (.setSelectionRange input (count text) (count text))
  (grow-input! input))

(defn- recall!
  "Steps through the shown tab's history of sent inputs, `step` -1 back
  and 1 forward, keeping what was being typed to come back to."
  [^js input {:keys [history-at draft]} history step]
  (let [at @history-at
        target (cond
                 (and (nil? at) (neg? step) (seq history)) (dec (count history))
                 (nil? at) nil
                 (neg? step) (max 0 (dec at))
                 (< (inc at) (count history)) (inc at)
                 :else ::draft)]
    (cond
      (= ::draft target) (do (reset! history-at nil) (set-input! input @draft))
      (some? target) (do (when (nil? at) (reset! draft (.-value input)))
                         (reset! history-at target)
                         (set-input! input (nth history target))))))

(defn- ready-to-send?
  "True when Enter should send `text`, as it always does in a shell tab
  and does in a REPL tab once its forms are closed."
  [kind text]
  (or (string/blank? text)
      (= :shell kind)
      (forms-state/complete? (editing-state/text-lines text))))

(defn- on-key-down!
  "Answers the keys typed into the pane, keeping those it answers from
  the cloud and VS Code. Other keys reach VS Code's keybindings, such as
  the command palette's, and the cloud passes over keys typed into text."
  [dispatch ^js input {:keys [history-at history-of kind-of] :as recall-state} ^js event]
  (let [text (.-value input)
        start (.-selectionStart input)
        end (.-selectionEnd input)
        typed-key (.-key event)
        answered! (fn []
                    (.preventDefault event)
                    (.stopPropagation event))]
    (cond
      (and (= "Enter" typed-key) (not (.-shiftKey event)) (ready-to-send? (kind-of) text))
      (do (answered!)
          ;; An empty line still goes, as Enter answering a prompt.
          (dispatch [:repl-submit text])
          (reset! history-at nil)
          (set-input! input ""))

      (and (= "ArrowUp" typed-key) (not (string/includes? (subs text 0 start) "\n")))
      (do (answered!) (recall! input recall-state (history-of) -1))

      (and (= "ArrowDown" typed-key) (not (string/includes? (subs text end) "\n")))
      (do (answered!) (recall! input recall-state (history-of) 1))

      (and (.-ctrlKey event) (= "c" (string/lower-case typed-key)) (= start end))
      (do (answered!) (dispatch [:repl-interrupt]))

      (= "Escape" typed-key)
      (do (answered!) (.blur input)))))

;; Moving and resizing the pane

(def ^:private smallest-width 260)

(def ^:private smallest-height 150)

(defn- pin-to-corner!
  "Anchors the pane by its top left corner where it now is, so dragging
  and resizing move the edges they hold."
  [^js root]
  (let [style (.-style root)]
    (when (string/blank? (.-top style))
      (set! (.-top style) (str (.-offsetTop root) "px"))
      (set! (.-left style) (str (.-offsetLeft root) "px"))
      (set! (.-bottom style) "auto"))))

(defn- current-geometry [^js root]
  {:left (.-offsetLeft root) :top (.-offsetTop root) :width (.-offsetWidth root) :height (.-offsetHeight root)})

(defn- place!
  "Sets the pane's place and size, kept within the window."
  [^js root {:keys [left top width height]}]
  (let [style (.-style root)
        window-width js/window.innerWidth
        window-height js/window.innerHeight
        fitted-width (max smallest-width (min width window-width))
        fitted-height (max smallest-height (min height window-height))]
    (set! (.-width style) (str fitted-width "px"))
    (set! (.-height style) (str fitted-height "px"))
    (set! (.-left style) (str (max 0 (min left (- window-width fitted-width))) "px"))
    (set! (.-top style) (str (max 0 (min top (- window-height fitted-height))) "px"))
    (set! (.-bottom style) "auto")))

(defn- keep-in-window!
  "Moves a pane pinned by its corner back inside a window that changed
  size, as when VS Code's panel opens below the cloud, returning it to the
  place the person chose once there is room again."
  [^js root placed]
  (when-not (string/blank? (.. root -style -top))
    (place! root (or @placed (current-geometry root)))))

(defn- follow-pointer!
  "Calls `move!` with how far the pointer pressed on `handle` has moved,
  then `done!` once it rises."
  [^js handle ^js event move! done!]
  (let [start-x (.-clientX event)
        start-y (.-clientY event)
        on-move (fn [^js move-event] (move! (- (.-clientX move-event) start-x) (- (.-clientY move-event) start-y)))
        stop! (fn stop! [_]
                (.removeEventListener handle "pointermove" on-move)
                (.removeEventListener handle "pointerup" stop!)
                (.removeEventListener handle "pointercancel" stop!)
                (done!))]
    (.setPointerCapture handle (.-pointerId event))
    (.addEventListener handle "pointermove" on-move)
    (.addEventListener handle "pointerup" stop!)
    (.addEventListener handle "pointercancel" stop!)))

(defn- start-drag!
  "Moves the pane with the pointer pressed on its header, keeping the
  header in view, and remembers where it lands."
  [dispatch ^js root ^js header ^js event]
  (when (and (zero? (.-button event)) (not (.closest (.-target event) "button")))
    (.preventDefault event)
    (pin-to-corner! root)
    (let [header-margin 40
          start-left (.-offsetLeft root)
          start-top (.-offsetTop root)]
      (follow-pointer! header event
                       (fn [dx dy]
                         (set! (.. root -style -left)
                               (str (max (- header-margin (.-offsetWidth root))
                                         (min (+ start-left dx) (- js/window.innerWidth header-margin)))
                                    "px"))
                         (set! (.. root -style -top)
                               (str (max 0 (min (+ start-top dy) (- js/window.innerHeight header-margin))) "px")))
                       #(dispatch [:save-repl-geometry (current-geometry root)])))))

(defn- start-resize!
  "Resizes the pane from the edges `edges` names, a set of :n, :s, :e,
  and :w, within the window, and remembers the size it ends at."
  [dispatch ^js root edges ^js handle ^js event]
  (when (zero? (.-button event))
    (.preventDefault event)
    (.stopPropagation event)
    (pin-to-corner! root)
    (let [{:keys [left top width height]} (current-geometry root)
          right (+ left width)
          bottom (+ top height)]
      (follow-pointer! handle event
                       (fn [dx dy]
                         (let [new-left (if (contains? edges :w) (max 0 (min (+ left dx) (- right smallest-width))) left)
                               new-top (if (contains? edges :n) (max 0 (min (+ top dy) (- bottom smallest-height))) top)
                               new-right (if (contains? edges :e) (min js/window.innerWidth (max (+ right dx) (+ new-left smallest-width))) right)
                               new-bottom (if (contains? edges :s) (min js/window.innerHeight (max (+ bottom dy) (+ new-top smallest-height))) bottom)]
                           (place! root {:left new-left :top new-top :width (- new-right new-left) :height (- new-bottom new-top)})))
                       #(dispatch [:save-repl-geometry (current-geometry root)])))))

(def ^:private resize-edges
  {"n" #{:n} "s" #{:s} "e" #{:e} "w" #{:w} "ne" #{:n :e} "nw" #{:n :w} "se" #{:s :e} "sw" #{:s :w}})

;; Tabs

(defn- tab-label
  "A tab's few words, what a REPL runs and its port, Calva for a tab
  joined to Calva's REPL, or a shell's name."
  [{:keys [kind status message label port via]}]
  (cond
    (= :shell kind) (or label "Shell")
    (= :calva via) (if (= :connecting status) "Calva…" "Calva")
    :else
    (case status
      :connected (if-let [[_ runtime port-text] (some->> message (re-matches #"(\S+).* on port (\d+)"))]
                   (str runtime " " port-text)
                   (str "REPL " port))
      :connecting "REPL…"
      "REPL")))

(defn- tab-title
  "A tab's tooltip, the connection's whole message or a shell's folder."
  [{:keys [kind message cwd label]}]
  (if (= :shell kind)
    (str (or label "Shell") (when cwd (str " in " cwd)))
    (or message "REPL")))

(defn- tabs-summary [{:keys [tabs active sessions]}]
  [active (mapv (fn [tab] (let [tab-session (get sessions tab)]
                            [tab (:kind tab-session) (:status tab-session) (tab-label tab-session) (tab-title tab-session)]))
                tabs)])

(defn- draw-tabs!
  "Rebuilds the tab strip with a button for each tab, its status light and
  a close button beside it, scrolling the strip to show the active tab
  when the tabs outgrow it."
  [dispatch ^js strip {:keys [tabs active sessions]}]
  (dom/replace-children!
   strip
   (for [tab tabs
         :let [{:keys [kind status] :as tab-session} (get sessions tab)]]
     [:div {:class ["repl-tab" (str "is-" (name kind)) (str "is-" (name (or status :idle))) (when (= tab active) "is-active")]
            :role "tab"
            :aria-selected (str (= tab active))
            :title (tab-title tab-session)}
      [:button.repl-tab-select {:on-click #(dispatch [:repl-select-tab tab])}
       [:span.repl-light {:aria-hidden "true"}]
       [:span.repl-tab-label (tab-label tab-session)]]
      [:button.repl-tab-close {:title "Close this tab" :aria-label (str "Close " (tab-label tab-session))
                               :on-click #(dispatch [:repl-close-tab tab])}
       "×"]]))
  (some-> (dom/find-in strip ".repl-tab.is-active")
          (.scrollIntoView #js {:block "nearest" :inline "nearest"})))

(defn- scroll-tabs! [^js strip ^js event]
  (when (and (zero? (.-deltaX event)) (> (.-scrollWidth strip) (.-clientWidth strip)))
    (.preventDefault event)
    (set! (.-scrollLeft strip) (+ (.-scrollLeft strip) (.-deltaY event)))))

(defn- draw-menu!
  "Fills the menu of new tabs with a REPL, Calva's REPL when Calva is
  there, a shell in a new terminal, and each terminal VS Code has open."
  [dispatch ^js menu terminals calva?]
  (dom/replace-children!
   menu
   (list
    [:button.repl-menu-item {:on-click #(dispatch [:repl-new-tab :repl])} "REPL"]
    (when calva?
      [:button.repl-menu-item {:on-click #(dispatch [:repl-new-tab :calva])} "Calva's REPL"])
    [:button.repl-menu-item {:on-click #(dispatch [:repl-new-tab :shell])} "Shell"]
    (when (seq terminals)
      (list [:div.repl-menu-heading "VS Code terminals"]
            (for [{terminal-name :name :keys [id]} terminals]
              [:button.repl-menu-item {:on-click #(dispatch [:repl-new-tab :shell id])} terminal-name]))))))

;; The panel

(def panel
  {:id :repl
   :mount (fn [dispatch]
            (let [root (dom/element
                        [:section.hud-repl {:aria-label "REPL and shells"}
                         [:header.repl-header
                          [:div.repl-tabs {:role "tablist"}]
                          [:button.repl-add {:title "Open a REPL or a shell" :aria-label "Open a REPL or a shell"
                                             :on-click #(dispatch [:repl-menu])}
                           icons/plus]
                          [:span.repl-spacer]
                          [:button.text-button.repl-connect {:on-click #(dispatch [:repl-connect])} "Connect"]
                          [:button.icon-button.repl-tool.repl-load
                           {:title "Load/Evaluate current file and its requires/dependencies"
                            :aria-label "Load/Evaluate current file and its requires/dependencies"
                            :on-click #(dispatch [:repl-load-following])}
                           icons/load-requires]
                          [:button.icon-button.repl-tool.repl-disconnect
                           {:title "Disconnect" :aria-label "Disconnect" :on-click #(dispatch [:repl-disconnect])}
                           icons/disconnect]
                          [:button.icon-button.repl-tool.repl-interrupt
                           {:title "Interrupt (Ctrl+C)" :aria-label "Interrupt" :on-click #(dispatch [:repl-interrupt])}
                           icons/interrupt]
                          [:button.icon-button.repl-tool {:title "Clear" :aria-label "Clear the log"
                                                          :on-click #(dispatch [:repl-clear])}
                           icons/clear-log]
                          [:button.icon-button.repl-tool {:title "Close (R)" :aria-label "Close the pane"
                                                          :on-click #(dispatch [:toggle-repl])}
                           icons/close]]
                         [:div.repl-menu {:role "menu"}]
                         [:div.repl-log {:role "log" :aria-label "Output" :tabindex "0"
                                         :on-click (fn [^js event]
                                                     (when-let [^js link (.closest (.-target event) ".repl-link")]
                                                       (.preventDefault event)
                                                       (dispatch [:open-window {:kind :web :source (.-textContent link)}])))}]
                         [:p.repl-empty "Open a REPL or a shell with +."]
                         [:label.repl-entry
                          [:span.repl-prompt]
                          [:textarea.repl-input {:rows "1" :spellcheck "false" :autocomplete "off" :autocapitalize "off"
                                                 :wrap "soft" :aria-label "Input"}]]
                         (for [[edge-name _edges] resize-edges]
                           [:div {:class ["repl-resize" (str "is-" edge-name)] :aria-hidden "true"
                                  :data-edge edge-name}])])
                  ^js header (dom/find-in root ".repl-header")
                  ^js input (dom/find-in root ".repl-input")
                  shown (atom {:history [] :kind nil})
                  placed (atom nil)
                  recall-state {:history-at (atom nil) :draft (atom "")
                                :history-of #(:history @shown)
                                :kind-of #(:kind @shown)}]
              (.addEventListener root "pointerdown" #(pin-to-corner! root) #js {:capture true})
              (.addEventListener header "pointerdown" #(start-drag! dispatch root header %))
              (doseq [^js handle (array-seq (.querySelectorAll root ".repl-resize"))]
                (.addEventListener handle "pointerdown"
                                   #(start-resize! dispatch root (get resize-edges (.. handle -dataset -edge)) handle %)))
              (.addEventListener js/window "resize" #(keep-in-window! root placed))
              (.addEventListener input "keydown" #(on-key-down! dispatch input recall-state %))
              (.addEventListener input "input" #(grow-input! input))
              (let [^js strip (dom/find-in root ".repl-tabs")]
                (.addEventListener strip "wheel" #(scroll-tabs! strip %) #js {:passive false}))
              {:root root
               :dispatch dispatch
               :input input
               :shown shown
               :recall-state recall-state
               :rendered (atom [])
               :synced (atom nil)
               :drawn-tabs (atom nil)
               :drawn-menu (atom nil)
               :placed placed
               :was-open? (atom false)}))
   :update (fn [{:keys [^js root dispatch ^js input shown recall-state rendered synced drawn-tabs drawn-menu placed was-open?]}
                {:keys [repl editing] :as state}]
             (let [{:keys [open? active geometry menu-open? terminals calva? following]} repl
                   {:keys [kind status log pending ns] :as tab-session} (repl-state/active-session state)
                   connected? (= :connected status)]
               (dom/toggle-class! root "is-open" open?)
               (dom/toggle-class! root "has-tabs" (some? tab-session))
               (doseq [shown-status [:connecting :connected :failed]]
                 (dom/toggle-class! root (str "is-" (name shown-status)) (and (= :repl kind) (= shown-status status))))
               (when (and geometry (not= geometry @placed))
                 (reset! placed geometry)
                 (place! root geometry))
               (when open?
                 (let [summary (tabs-summary repl)]
                   (when (not= summary @drawn-tabs)
                     (reset! drawn-tabs summary)
                     (draw-tabs! dispatch (dom/find-in root ".repl-tabs") repl)))
                 (dom/toggle-class! (dom/find-in root ".repl-menu") "is-open" menu-open?)
                 (when (and menu-open? (not= [terminals calva?] @drawn-menu))
                   (reset! drawn-menu [terminals calva?])
                   (draw-menu! dispatch (dom/find-in root ".repl-menu") terminals calva?)))
               (dom/toggle-class! (dom/find-in root ".repl-connect") "is-hidden"
                                  (or (not= :repl kind) (contains? #{:connected :connecting} status)))
               (dom/toggle-class! (dom/find-in root ".repl-disconnect") "is-hidden"
                                  (or (not= :repl kind) (not (contains? #{:connected :connecting} status))))
               (dom/toggle-class! (dom/find-in root ".repl-load") "is-hidden" (or (not= :repl kind) (nil? following)))
               (set! (.-disabled (dom/find-in root ".repl-interrupt"))
                     (not (or (and (= :repl kind) (seq pending)) (and (= :shell kind) (= :running status)))))
               (dom/set-text! (dom/find-in root ".repl-prompt")
                              (string/trimr (if (= :shell kind) (repl-state/shell-prompt tab-session) (repl-state/prompt ns))))
               ;; While a command runs, what is typed goes to it, which prints its own prompt.
               (dom/toggle-class! (dom/find-in root ".repl-prompt") "is-hidden"
                                  (if (= :shell kind) (= :running status) (not connected?)))
               (.setAttribute input "placeholder" (cond
                                                    (and (= :shell kind) (= :running status)) "A line for the command running"
                                                    (= :shell kind) (str "A command for " (or (:label tab-session) "the shell"))
                                                    connected? "Clojure to evaluate"
                                                    (= :calva (:via tab-session)) "Clojure to evaluate in Calva's REPL"
                                                    :else "A port, such as 7888"))
               (when (not= active (:tab @shown))
                 (reset! (:history-at recall-state) nil)
                 (reset! (:draft recall-state) "")
                 (reset! rendered [])
                 (reset! synced nil)
                 (set! (.-textContent (dom/find-in root ".repl-log")) ""))
               (reset! shown {:tab active :history (:history tab-session []) :kind kind})
               (when (and open? (not (identical? log @synced)))
                 (reset! synced log)
                 (sync-log! (dom/find-in root ".repl-log") rendered (or log [])))
               (when (and open? (not @was-open?) (not editing))
                 (js/requestAnimationFrame #(.focus input #js {:preventScroll true})))
               (when (and (not open?) @was-open? (identical? input (.-activeElement js/document)))
                 (.blur input))
               (reset! was-open? open?)))})
