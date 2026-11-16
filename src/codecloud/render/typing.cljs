(ns codecloud.render.typing
  "Typing into the cloud. While a file is edited, a hidden text area holds
  the keyboard: it turns keys, typed text, input methods, and the
  clipboard into edits, keeps the selected text so copying works however
  the host routes it, and sits at the caret so input methods open beside
  it."
  (:require [clojure.string :as string]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.render.dom :as dom]))

(defn mac?
  "True on macOS, where Cmd takes the place of Ctrl."
  []
  (string/includes? (or (.-platform js/navigator) "") "Mac"))

(defn- evaluation-scope
  "The scope Enter evaluates with `event`'s modifiers, as Calva binds
  them, or nil for Enter and Shift+Enter, which break the line. Cmd
  stands for Ctrl on macOS."
  [^js event]
  (let [control? (or (.-ctrlKey event) (.-metaKey event))
        shift? (.-shiftKey event)
        alt? (.-altKey event)]
    (cond
      (and control? alt? shift?) :file-to-caret
      (and control? alt?) :to-caret
      (and control? shift?) :enclosing
      control? :form
      (and alt? shift?) :top-level-to-caret
      alt? :top-level)))

(defn- key-edit
  "The event a key press makes while editing, or nil for keys left to the
  browser and VS Code, such as copying and the command palette. Enter
  with modifiers evaluates, and Tab formats Clojure."
  [^js event on-mac?]
  (let [shift? (.-shiftKey event)
        command? (if on-mac? (.-metaKey event) (.-ctrlKey event))
        word? (if on-mac? (.-altKey event) (.-ctrlKey event))
        line-key? (and on-mac? (.-metaKey event))
        typed-key (string/lower-case (.-key event))]
    (case (.-key event)
      "ArrowLeft" [:edit :move (cond line-key? :line-start word? :word-left :else :left) shift?]
      "ArrowRight" [:edit :move (cond line-key? :line-end word? :word-right :else :right) shift?]
      "ArrowUp" [:edit :move (if line-key? :file-start :up) shift?]
      "ArrowDown" [:edit :move (if line-key? :file-end :down) shift?]
      "Home" [:edit :move (if command? :file-start :line-start) shift?]
      "End" [:edit :move (if command? :file-end :line-end) shift?]
      "PageUp" [:edit :move :page-up shift?]
      "PageDown" [:edit :move :page-down shift?]
      "Backspace" [:edit :delete (if word? :word-left :left)]
      "Delete" [:edit :delete (if word? :word-right :right)]
      "Enter" (if-let [scope (evaluation-scope event)] [:evaluate scope] [:edit :break-line])
      "Tab" (when-not command? (if shift? [:edit :outdent] [:tab]))
      "Escape" [:end-editing]
      (if (and shift? (.-altKey event) (not command?) (= "KeyF" (.-code event)))
        [:format-code :file]
        (when (and command? (not (.-altKey event)))
          (case typed-key
            "z" (if shift? [:edit :redo] [:edit :undo])
            "y" (when-not on-mac? [:edit :redo])
            "a" [:edit :select-all]
            "s" [:save-file]
            "/" [:edit :comment]
            nil))))))

(defn- chord-start?
  "True for Ctrl+Alt+C, which begins Calva's chords. Where those keys
  type a character, as AltGr+C does on some keyboards, the character
  wins."
  [^js event on-mac?]
  (and (.-ctrlKey event) (.-altKey event) (not (.-shiftKey event)) (not (.-metaKey event))
       (= "KeyC" (.-code event))
       (or on-mac? (= "c" (string/lower-case (.-key event))))))

(defn- chord-edit [^js event]
  (cond
    (= "Enter" (.-key event)) [:evaluate (if (.-shiftKey event) :file-with-requires :file)]
    (and (.-ctrlKey event) (.-altKey event) (= "KeyD" (.-code event))) [:interrupt-evaluations]))

(defn- modifier-only?
  "True for a key press of a modifier alone, which leaves a chord
  waiting."
  [^js event]
  (contains? #{"Control" "Alt" "AltGraph" "Shift" "Meta"} (.-key event)))

(defn- mirror!
  "Puts the selected text, or the caret's whole line when nothing is
  selected, into the text area and selects it, so the browser's own copy
  and cut take it."
  [^js area buffer]
  (let [held (if (editing-state/selected? buffer)
               (editing-state/selected-text buffer)
               (str (get (:lines buffer) (first (:caret buffer)) "") "\n"))]
    (when (not= held (.-value area))
      (set! (.-value area) held))
    (.setSelectionRange area 0 (count held))))

(defn focused?
  "True while the text area holds the keyboard."
  [{:keys [^js area]}]
  (identical? area (.-activeElement js/document)))

(defn focus!
  "Gives the keyboard back to the text area, after a click on the page."
  [{:keys [^js area]}]
  (.focus area #js {:preventScroll true}))

(defn- take-key! [^js event]
  (.preventDefault event)
  (.stopPropagation event))

(defn- end-chord! [dispatch chord-waiting?]
  (when @chord-waiting?
    (reset! chord-waiting? false)
    (dispatch [:chord nil])))

(defn- key-down!
  "Turns a key pressed in the text area into an edit, the chord's first key
  into a wait for its second, and that second key into the chord's edit."
  [dispatch {:keys [composing? chord-waiting? on-mac?]} ^js event]
  (let [;; What browsers report as the key code while an input method composes.
        composition-key-code 229]
    (cond
      (or @composing? (.-isComposing event) (= composition-key-code (.-keyCode event))) nil

      ;; The key after the chord's first is the chord's, as in VS Code.
      @chord-waiting?
      (when-not (modifier-only? event)
        (take-key! event)
        (end-chord! dispatch chord-waiting?)
        (some-> (chord-edit event) dispatch))

      (chord-start? event on-mac?)
      (do (take-key! event)
          (reset! chord-waiting? true)
          (dispatch [:chord "Ctrl+Alt+C"]))

      :else
      (when-let [edit (key-edit event on-mac?)]
        (take-key! event)
        (dispatch edit)))))

(defn create-input
  "A hidden text area in `container` turning typing into events for
  `dispatch`. Whatever it held is selected, so its value after typing is
  exactly the text typed."
  [^js container dispatch current-buffer]
  (let [^js area (dom/element [:textarea.edit-input {:aria-label "Edit code"
                                                     :spellcheck "false"
                                                     :autocomplete "off"
                                                     :autocapitalize "off"
                                                     :wrap "off"
                                                     :tabindex "-1"}])
        composing? (atom false)
        chord-waiting? (atom false)
        key-state {:composing? composing? :chord-waiting? chord-waiting? :on-mac? (mac?)}
        typed! (fn [text]
                 (when (seq text) (dispatch [:edit :type text]))
                 (when-let [buffer (current-buffer)] (mirror! area buffer)))
        clipboard! (fn [^js event]
                     (.preventDefault event)
                     (.setData (.-clipboardData event) "text/plain" (.-value area)))]
    (.addEventListener area "keydown" #(key-down! dispatch key-state %))
    (.addEventListener area "blur" (fn [_] (end-chord! dispatch chord-waiting?)))
    (.addEventListener area "input"
                       (fn [_] (when-not @composing? (typed! (.-value area)))))
    (.addEventListener area "compositionstart" (fn [_] (reset! composing? true)))
    (.addEventListener area "compositionend"
                       (fn [^js event]
                         (reset! composing? false)
                         (typed! (.-data event))))
    (.addEventListener area "copy"
                       (fn [^js event]
                         (when (current-buffer) (clipboard! event))))
    (.addEventListener area "cut"
                       (fn [^js event]
                         (when-let [buffer (current-buffer)]
                           (clipboard! event)
                           ;; Without a selection, cutting takes the caret's line.
                           (when-not (editing-state/selected? buffer)
                             (dispatch [:edit :select-line (:caret buffer)]))
                           (dispatch [:edit :cut]))))
    (.addEventListener area "paste"
                       (fn [^js event]
                         (.preventDefault event)
                         (let [text (.getData (.-clipboardData event) "text/plain")]
                           (when (seq text) (dispatch [:edit :paste text])))))
    (.appendChild container area)
    {:area area :composing? composing? :editing-path (atom nil)}))

(defn sync-input!
  "Takes the keyboard when editing starts, lets it go when editing stops,
  and keeps the selected text in the text area."
  [{:keys [^js area composing? editing-path] :as input} {:keys [editing buffers]}]
  (let [path (:path editing)
        buffer (get buffers path)]
    (cond
      (and path buffer)
      (do (when-not @composing? (mirror! area buffer))
          (when (not= path @editing-path)
            (focus! input))
          (reset! editing-path path))

      @editing-path
      (do (reset! editing-path nil)
          (set! (.-value area) "")
          (.blur area)))))

(defn place!
  "Sets the text area at the caret, so input methods open beside it, with
  its text at the size of the code."
  [{:keys [^js area]} [x y] line-pixels]
  (let [style (.-style area)]
    (set! (.-left style) (str (js/Math.round x) "px"))
    (set! (.-top style) (str (js/Math.round (- y (* 0.5 line-pixels))) "px"))
    (set! (.-fontSize style) (str (max 8 (js/Math.round (* 0.8 line-pixels))) "px"))))
