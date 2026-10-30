(ns codecloud.render.typing
  "Typing into the cloud. While a file is edited, a hidden text area holds
  the keyboard: it turns keys, typed text, input methods, and the
  clipboard into edits, keeps the selected text so copying works however
  the host routes it, and sits at the caret so input methods open beside
  it."
  (:require [clojure.string :as string]
            [codecloud.presentation.editing :as editing]
            [codecloud.render.dom :as dom]))

(defn- mac? []
  (string/includes? (or (.-platform js/navigator) "") "Mac"))

(defn- key-edit
  "The event a key press makes while editing, or nil for keys the editor
  leaves to the browser and to VS Code, such as copying and the command
  palette. Word moves take Ctrl, or Alt on macOS, and commands take Ctrl,
  or Cmd on macOS."
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
      "Enter" (when-not command? [:edit :break-line])
      "Tab" (when-not command? (if shift? [:edit :outdent] [:edit :indent]))
      "Escape" [:end-editing]
      (when (and command? (not (.-altKey event)))
        (case typed-key
          "z" (if shift? [:edit :redo] [:edit :undo])
          "y" (when-not on-mac? [:edit :redo])
          "a" [:edit :select-all]
          "s" [:save-file]
          "/" [:edit :comment]
          nil)))))

(defn- mirror!
  "Puts the selected text, or the caret's whole line when nothing is
  selected, into the text area and selects it, so the browser's own copy
  and cut take it."
  [^js area buffer]
  (let [held (if (editing/selected? buffer)
               (editing/selected-text buffer)
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

(defn create-input
  "A hidden text area in `container` that turns typing into events for
  `dispatch`. `current-buffer` gives the buffer being edited, or nil.
  Whatever the area held was selected, so its value after typing is
  exactly the text typed."
  [^js container dispatch current-buffer]
  (let [^js area (dom/element [:textarea.edit-input {:aria-label "Edit code"
                                                     :spellcheck "false"
                                                     :autocomplete "off"
                                                     :autocapitalize "off"
                                                     :wrap "off"
                                                     :tabindex "-1"}])
        composing? (atom false)
        on-mac? (mac?)
        typed! (fn [text]
                 (when (seq text) (dispatch [:edit :type text]))
                 (when-let [buffer (current-buffer)] (mirror! area buffer)))
        clipboard! (fn [^js event]
                     (.preventDefault event)
                     (.setData (.-clipboardData event) "text/plain" (.-value area)))]
    (.addEventListener area "keydown"
                       (fn [^js event]
                         (when-not (or @composing? (.-isComposing event) (= 229 (.-keyCode event)))
                           (when-let [edit (key-edit event on-mac?)]
                             (.preventDefault event)
                             (.stopPropagation event)
                             (dispatch edit)))))
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
                           (when-not (editing/selected? buffer)
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
