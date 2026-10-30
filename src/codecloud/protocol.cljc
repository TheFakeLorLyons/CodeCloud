(ns codecloud.protocol
  "Messages between the cloud view and its host. Each is an EDN map with a
  :type key, sent as a string so keywords and sets survive the trip."
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])))

(def message-types
  "Every message type, with its direction and what it carries."
  {:view/ready "view to host: the view loaded and can receive data"
   :view/select-snapshot "view to host: show :snapshot-id, a commit hash or :working-tree"
   :view/request-sources "view to host: send the source files of :namespaces, each {:namespace-id :source-key}, in :snapshot-id"
   :view/open-location "view to host: open :path at :line and :column in a text editor"
   :view/edit "view to host: in :path, replace :replaced at :range, [line column end-line end-column] from zero, with :text; :revision numbers the view's edits and :epoch the text they build on"
   :view/save "view to host: save :path"
   :view/save-preference "view to host: remember :key with :value for this viewer"
   :view/save-view "view to host: add a view called :name with :selection to the cloud file"
   :view/log "view to host: write :message to the extension's output channel"
   :host/init "host to view: settings, theme, editor font, and remembered preferences"
   :host/views "host to view: the cloud file's named :views and its :color-layers"
   :host/snapshot "host to view: a snapshot to draw"
   :host/history "host to view: commits that can be viewed as :commits"
   :host/sources "host to view: :entries of {:source-key :namespace-id :files}, each file with :path, :language, and :text"
   :host/status "host to view: progress :message, or nil when idle"
   :host/theme "host to view: the editor theme changed to :theme-kind"
   :host/editor-colors "host to view: the editor theme's :code-colors for both skies, or nil when its files cannot be read"
   :host/arrive "host to view: the viewer zoomed out of :path at :line; start there"
   :host/shown "host to view: the cloud's panel came back into view"
   :host/edited "host to view: edit :revision of :path applied, leaving text with :content-key, and whether the file is :dirty?"
   :host/document "host to view: :path holds :text with :content-key, after an edit that could not apply or a change from elsewhere; later edits build on :epoch"
   :host/dirty "host to view: whether :path has unsaved changes"
   :host/notice "host to view: a :message worth showing, such as configuration problems"})

(defn encode
  "Message map to string."
  [message]
  (let [protocol-version 1]
    (pr-str (assoc message :protocol/version protocol-version))))

(defn decode
  "String to message map, or nil when the text is not a message."
  [text]
  (try
    (let [message #?(:clj (edn/read-string text)
                     :cljs (reader/read-string text))]
      (when (and (map? message) (keyword? (:type message)))
        message))
    (catch #?(:clj Exception :cljs :default) _error
      nil)))

(defn message
  "A message of `message-type`, which must appear in `message-types`, with
  the given fields."
  [message-type fields]
  {:pre [(contains? message-types message-type)]}
  (assoc fields :type message-type))
