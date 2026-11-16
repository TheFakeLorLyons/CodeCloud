(ns codecloud.render.hud.repl
  "The REPL pane: a small window over the sky holding the REPL's log and a
  line to type code into. Its header moves it and its corner resizes it.
  Enter sends what was typed once its forms are closed, the arrow keys
  walk back through what was sent, and Ctrl+C with nothing selected
  interrupts."
  (:require [clojure.string :as string]
            [codecloud.presentation.editing :as editing]
            [codecloud.presentation.forms :as forms]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

;; The log

(defn- entry-element
  "An element showing one log entry, without the line break most output
  ends with."
  [{:keys [kind text]}]
  (dom/element [:div {:class ["repl-line" (str "is-" (name kind))]} (string/replace text #"\n$" "")]))

(defn- shown-entry
  "Appends an element for `entry` to the log, returning what the pane
  keeps of it."
  [^js log-element entry]
  (let [node (entry-element entry)]
    (.appendChild log-element node)
    {:number (:number entry) :text (:text entry) :node node}))

(defn- sync-log!
  "Brings the log's elements in line with the log: dropping entries it no
  longer keeps, growing the last one shown, and adding new ones, so a
  stream of output costs little. Follows the newest entry when the log
  was scrolled to its end."
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

(defn- grow-input!
  "Fits the text area's height to its lines, up to a few."
  [^js input]
  (let [tallest-pixels 96]
    (set! (.. input -style -height) "auto")
    (set! (.. input -style -height) (str (min tallest-pixels (.-scrollHeight input)) "px"))))

(defn- set-input!
  "Puts `text` into the text area with the caret at its end."
  [^js input text]
  (set! (.-value input) text)
  (.setSelectionRange input (count text) (count text))
  (grow-input! input))

(defn- recall!
  "Steps through the history of sent inputs, `step` -1 back and 1
  forward, keeping what was being typed to come back to."
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

(defn- on-key-down!
  "Answers the keys typed into the pane, keeping them from the cloud."
  [dispatch ^js input {:keys [history-at history-of] :as recall-state} ^js event]
  (let [text (.-value input)
        start (.-selectionStart input)
        end (.-selectionEnd input)
        typed-key (.-key event)]
    (cond
      (and (= "Enter" typed-key) (not (.-shiftKey event))
           (or (string/blank? text) (forms/complete? (editing/text-lines text))))
      (do (.preventDefault event)
          (when-not (string/blank? text)
            (dispatch [:repl-submit text]))
          (reset! history-at nil)
          (set-input! input ""))

      (and (= "ArrowUp" typed-key) (not (string/includes? (subs text 0 start) "\n")))
      (do (.preventDefault event) (recall! input recall-state (history-of) -1))

      (and (= "ArrowDown" typed-key) (not (string/includes? (subs text end) "\n")))
      (do (.preventDefault event) (recall! input recall-state (history-of) 1))

      (and (.-ctrlKey event) (= "c" (string/lower-case typed-key)) (= start end))
      (do (.preventDefault event) (dispatch [:repl-interrupt]))

      (= "Escape" typed-key)
      (do (.preventDefault event) (.blur input)))
    (.stopPropagation event)))

;; Moving the pane

(defn- pin-to-corner!
  "Anchors the pane by its top left corner where it now is, so dragging
  and resizing move the edges they hold."
  [^js root]
  (let [style (.-style root)]
    (when (string/blank? (.-top style))
      (set! (.-top style) (str (.-offsetTop root) "px"))
      (set! (.-left style) (str (.-offsetLeft root) "px"))
      (set! (.-bottom style) "auto"))))

(defn- start-drag!
  "Moves the pane with the pointer pressed on its header, keeping its
  header in view."
  [^js root ^js header ^js event]
  (when (and (zero? (.-button event)) (not (.closest (.-target event) "button")))
    (.preventDefault event)
    (pin-to-corner! root)
    (let [header-margin 40
          start-x (.-clientX event)
          start-y (.-clientY event)
          start-left (.-offsetLeft root)
          start-top (.-offsetTop root)
          move! (fn [^js move-event]
                  (let [left (+ start-left (- (.-clientX move-event) start-x))
                        top (+ start-top (- (.-clientY move-event) start-y))]
                    (set! (.. root -style -left)
                          (str (max (- header-margin (.-offsetWidth root)) (min left (- js/window.innerWidth header-margin))) "px"))
                    (set! (.. root -style -top)
                          (str (max 0 (min top (- js/window.innerHeight header-margin))) "px"))))
          stop! (fn stop! [_]
                  (.removeEventListener header "pointermove" move!)
                  (.removeEventListener header "pointerup" stop!)
                  (.removeEventListener header "pointercancel" stop!))]
      (.setPointerCapture header (.-pointerId event))
      (.addEventListener header "pointermove" move!)
      (.addEventListener header "pointerup" stop!)
      (.addEventListener header "pointercancel" stop!))))

;; The panel

(defn- status-text
  "The header's few words on the connection: what the server runs and
  where, once connected."
  [{:keys [status message]}]
  (case status
    :connected message
    :connecting "REPL connecting…"
    "REPL not connected"))

(def panel
  {:id :repl
   :mount (fn [dispatch]
            (let [root (dom/element
                        [:section.hud-repl {:aria-label "REPL"}
                         [:header.repl-header
                          [:span.repl-light {:aria-hidden "true"}]
                          [:span.repl-status {:aria-live "polite"}]
                          [:button.text-button.repl-connect {:on-click #(dispatch [:repl-connect])} "Connect"]
                          [:button.icon-button.repl-tool.repl-disconnect
                           {:title "Disconnect" :aria-label "Disconnect" :on-click #(dispatch [:repl-disconnect])}
                           icons/disconnect]
                          [:button.icon-button.repl-tool.repl-interrupt
                           {:title "Interrupt (Ctrl+C)" :aria-label "Interrupt" :on-click #(dispatch [:repl-interrupt])}
                           icons/interrupt]
                          [:button.icon-button.repl-tool {:title "Clear" :aria-label "Clear the log"
                                                          :on-click #(dispatch [:repl-clear])}
                           icons/clear-log]
                          [:button.icon-button.repl-tool {:title "Close (R)" :aria-label "Close the REPL"
                                                          :on-click #(dispatch [:toggle-repl])}
                           icons/close]]
                         [:div.repl-log {:role "log" :aria-label "REPL output" :tabindex "0"}]
                         [:label.repl-entry
                          [:span.repl-prompt]
                          [:textarea.repl-input {:rows "1" :spellcheck "false" :autocomplete "off" :autocapitalize "off"
                                                 :wrap "soft" :aria-label "REPL input"}]]])
                  ^js header (dom/find-in root ".repl-header")
                  ^js input (dom/find-in root ".repl-input")
                  history (atom [])
                  recall-state {:history-at (atom nil) :draft (atom "") :history-of #(deref history)}]
              (.addEventListener root "pointerdown" #(pin-to-corner! root) #js {:capture true})
              (.addEventListener header "pointerdown" #(start-drag! root header %))
              (.addEventListener input "keydown" #(on-key-down! dispatch input recall-state %))
              (.addEventListener input "input" #(grow-input! input))
              {:root root
               :input input
               :history history
               :rendered (atom [])
               :synced-log (atom nil)
               :was-open? (atom false)}))
   :update (fn [{:keys [^js root ^js input history rendered synced-log was-open?]} {:keys [repl editing]}]
             (let [{:keys [open? status ns log pending]} repl
                   connected? (= :connected status)]
               (dom/toggle-class! root "is-open" open?)
               (doseq [shown-status [:connecting :connected :failed]]
                 (dom/toggle-class! root (str "is-" (name shown-status)) (= shown-status status)))
               (dom/set-text! (dom/find-in root ".repl-status") (status-text repl))
               (dom/toggle-class! (dom/find-in root ".repl-connect") "is-hidden" (contains? #{:connected :connecting} status))
               (dom/toggle-class! (dom/find-in root ".repl-disconnect") "is-hidden" (not (contains? #{:connected :connecting} status)))
               (set! (.-disabled (dom/find-in root ".repl-interrupt")) (empty? pending))
               (dom/set-text! (dom/find-in root ".repl-prompt") (str (or ns "user") "=>"))
               (dom/toggle-class! (dom/find-in root ".repl-prompt") "is-hidden" (not connected?))
               (.setAttribute input "placeholder" (if connected? "Clojure to evaluate" "A port, such as 7888"))
               (reset! history (:history repl))
               (when (and open? (not (identical? log @synced-log)))
                 (reset! synced-log log)
                 (sync-log! (dom/find-in root ".repl-log") rendered log))
               (when (and open? (not @was-open?) (not editing))
                 (js/requestAnimationFrame #(.focus input #js {:preventScroll true})))
               (when (and (not open?) @was-open? (identical? input (.-activeElement js/document)))
                 (.blur input))
               (reset! was-open? open?)))})
