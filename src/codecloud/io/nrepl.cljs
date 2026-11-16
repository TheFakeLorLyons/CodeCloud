(ns codecloud.io.nrepl
  "A client for nREPL, the network REPL Clojure tools share. Messages
  travel as bencode. A connection clones a session of its own on the
  server and passes each reply to the request whose id it carries."
  (:require ["net" :as net]
            ["path" :as path]))

;; Bencode

(def ^:private integer-mark (.charCodeAt "i" 0))
(def ^:private list-mark (.charCodeAt "l" 0))
(def ^:private dictionary-mark (.charCodeAt "d" 0))
(def ^:private end-mark (.charCodeAt "e" 0))
(def ^:private length-mark (.charCodeAt ":" 0))
(def ^:private first-digit (.charCodeAt "0" 0))
(def ^:private last-digit (.charCodeAt "9" 0))

(defn- key-text
  "A dictionary key as text: a keyword's name with its namespace."
  [entry-key]
  (if (keyword? entry-key) (subs (str entry-key) 1) (str entry-key)))

(defn- encoded-pieces
  "The Buffers that together bencode `value`."
  [value]
  (cond
    (string? value) (let [text-bytes (js/Buffer.from value "utf8")]
                      [(js/Buffer.from (str (.-length text-bytes) ":")) text-bytes])
    (keyword? value) (encoded-pieces (key-text value))
    (integer? value) [(js/Buffer.from (str "i" value "e"))]
    (map? value) (concat [(js/Buffer.from "d")]
                         (mapcat (fn [[entry-key entry-value]]
                                   (concat (encoded-pieces (key-text entry-key)) (encoded-pieces entry-value)))
                                 (sort-by (comp key-text key) value))
                         [(js/Buffer.from "e")])
    (sequential? value) (concat [(js/Buffer.from "l")] (mapcat encoded-pieces value) [(js/Buffer.from "e")])
    :else (throw (ex-info (str "Bencode has no form for " (pr-str value)) {:value value}))))

(defn encode
  "The bencoded bytes of `value`, made of strings, keywords, integers,
  sequences, and maps, whose keys go out sorted as bencode asks."
  [value]
  (js/Buffer.concat (into-array (encoded-pieces value))))

(defn- decode-at
  "[value offset-after] for the value bencoded at `offset` in `buffer`,
  or nil when the buffer ends inside it. Text decodes as UTF-8 and
  dictionary keys as keywords."
  [^js buffer offset]
  (when (< offset (.-length buffer))
    (let [mark (aget buffer offset)]
      (cond
        (= integer-mark mark)
        (let [end (.indexOf buffer end-mark offset)]
          (when-not (neg? end)
            [(js/parseInt (.toString buffer "latin1" (inc offset) end) 10) (inc end)]))

        (= list-mark mark)
        (loop [items (transient []) position (inc offset)]
          (cond
            (>= position (.-length buffer)) nil
            (= end-mark (aget buffer position)) [(persistent! items) (inc position)]
            :else (when-let [[item after] (decode-at buffer position)]
                    (recur (conj! items item) after))))

        (= dictionary-mark mark)
        (loop [entries (transient {}) position (inc offset)]
          (cond
            (>= position (.-length buffer)) nil
            (= end-mark (aget buffer position)) [(persistent! entries) (inc position)]
            :else (when-let [[entry-key after-key] (decode-at buffer position)]
                    (when-let [[entry-value after-value] (decode-at buffer after-key)]
                      (recur (assoc! entries (keyword entry-key) entry-value) after-value)))))

        (<= first-digit mark last-digit)
        (let [colon (.indexOf buffer length-mark offset)]
          (when-not (neg? colon)
            (let [start (inc colon)
                  end (+ start (js/parseInt (.toString buffer "latin1" offset colon) 10))]
              (when (<= end (.-length buffer))
                [(.toString buffer "utf8" start end) end]))))

        :else (throw (ex-info "The bytes are not bencode" {:offset offset :byte mark}))))))

(defn decode-all
  "The whole values bencoded one after another at the start of `buffer`,
  as :values, and the bytes after them as :remainder, which begin a value
  still arriving."
  [^js buffer]
  (loop [values [] offset 0]
    (if-let [[value after] (decode-at buffer offset)]
      (recur (conj values value) after)
      {:values values :remainder (.subarray buffer offset)})))

;; Connections

(def ^:private printed-value-limit
  "The most bytes of a printed value the server sends, through its print
  middleware's quota, so one huge result cannot flood the view."
  32768)

(defn done?
  "True for the reply that ends a request."
  [reply]
  (boolean (some #(= "done" %) (:status reply))))

(defn- deliver-replies!
  "Passes every whole reply among the bytes received so far to the
  request awaiting it, or to `on-unclaimed`, keeping a reply still
  arriving for the next chunk."
  [{:keys [state]} chunk on-unclaimed]
  (let [{:keys [values remainder]} (decode-all (js/Buffer.concat #js [(:remainder @state) chunk]))]
    (swap! state assoc :remainder remainder)
    (doseq [reply values
            :when (map? reply)
            :let [id (:id reply)
                  on-reply (get-in @state [:awaiting id])]]
      (when (done? reply)
        (swap! state update :awaiting dissoc id))
      (cond
        on-reply (on-reply reply)
        on-unclaimed (on-unclaimed reply)))))

(defn request!
  "Sends `message` in the connection's session with a fresh id, unless it
  carries its own, and passes each reply to `on-reply` until one says it
  is done. Returns the id."
  [{:keys [^js socket state]} message on-reply]
  (let [id (or (:id message) (str "codecloud-" (:sent (swap! state update :sent inc))))
        session (:session @state)
        sent (cond-> (assoc message :id id)
               (and session (not (contains? message :session))) (assoc :session session))]
    (swap! state assoc-in [:awaiting id] on-reply)
    (.write socket (encode sent))
    id))

(defn- coded-error
  "An Error with `message` whose code is `code`, as Node's own errors
  carry."
  [message code]
  (js/Object.assign (js/Error. message) #js {:code code}))

(defn connect
  "Resolves to a connection to the nREPL server at `host` and `port`
  holding a session of its own, or rejects when nothing answers there, or
  when what answers does not clone a session within
  `answer-milliseconds`, 5000 unless given, as an nREPL server would.
  `on-close` gets the connection when it ends after opening, every
  request still awaiting replies having had a last one with the status
  connection-closed. `on-unclaimed` gets replies no request awaits, such
  as output printed after an evaluation ended."
  [{:keys [host port on-close on-unclaimed answer-milliseconds] :or {answer-milliseconds 5000}}]
  (js/Promise.
   (fn [resolve reject]
     (let [^js socket (.createConnection net #js {:host host :port port})
           state (atom {:remainder (js/Buffer.alloc 0) :awaiting {} :sent 0 :session nil})
           connection {:socket socket :state state :host host :port port}
           settled? (atom false)
           give-up! (fn [error]
                      (when-not @settled?
                        (reset! settled? true)
                        (.destroy socket)
                        (reject error)))
           timer (js/setTimeout #(give-up! (coded-error (str "No nREPL answer on port " port) "NREPL_SILENT"))
                                answer-milliseconds)]
       (.on socket "data" (fn [chunk]
                            (try
                              (deliver-replies! connection chunk on-unclaimed)
                              (catch :default error
                                (.destroy socket error)))))
       (.on socket "error" give-up!)
       (.on socket "close"
            (fn [_had-error]
              (js/clearTimeout timer)
              (let [awaiting (:awaiting @state)]
                (swap! state assoc :awaiting {})
                (doseq [[id on-reply] awaiting]
                  (on-reply {:id id :status ["done" "connection-closed"]})))
              (if (:session @state)
                (when on-close (on-close connection))
                (give-up! (coded-error "The connection closed before a session opened" "NREPL_SILENT")))))
       (.on socket "connect"
            (fn []
              (request! connection {:op "clone"}
                        (fn [{:keys [new-session]}]
                          (when (and new-session (not @settled?))
                            (reset! settled? true)
                            (js/clearTimeout timer)
                            (swap! state assoc :session new-session)
                            (resolve connection))))))))))

(defn evaluate!
  "Evaluates `code` in namespace `ns`, the session's own when nil, as read
  from `file` at `line` and `column` when given, so definitions and stack
  traces point there. Returns the request's id."
  [connection {:keys [id code ns file line column]} on-reply]
  (request! connection
            (cond-> {:op "eval" :code code :nrepl.middleware.print/quota printed-value-limit}
              id (assoc :id id)
              ns (assoc :ns ns)
              file (assoc :file file)
              line (assoc :line line)
              column (assoc :column column))
            on-reply))

(defn load-file!
  "Loads `code` as the whole of the file at `file`, an absolute path.
  Returns the request's id."
  [connection {:keys [id code file]} on-reply]
  (request! connection
            (cond-> {:op "load-file" :file code :file-path file :file-name (path/basename file)
                     :nrepl.middleware.print/quota printed-value-limit}
              id (assoc :id id))
            on-reply))

(defn interrupt!
  "Asks the server to stop the evaluation with id `interrupt-id`."
  [connection interrupt-id on-reply]
  (request! connection {:op "interrupt" :interrupt-id interrupt-id} on-reply))

(defn close!
  "Closes the session on the server, then the connection."
  [{:keys [^js socket] :as connection}]
  (when-not (or (.-destroyed socket) (.-writableEnded socket))
    (request! connection {:op "close"} (fn [_reply] nil))
    (.end socket)))
