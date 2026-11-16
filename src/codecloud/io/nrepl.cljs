(ns codecloud.io.nrepl
  "A client for nREPL, the network REPL Clojure tools share. Messages
  travel as bencode. A connection clones a session of its own on the
  server and passes each reply to the request whose id it carries."
  (:require ["net" :as net]
            ["path" :as path]
            [codecloud.io.task :as task]))

;; Bencode

(defn- key-text [entry-key]
  (if (keyword? entry-key) (subs (str entry-key) 1) (str entry-key)))

(defn- encoded-pieces [value]
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

(def ^:private integer-mark (.charCodeAt "i" 0))
(def ^:private list-mark (.charCodeAt "l" 0))
(def ^:private dictionary-mark (.charCodeAt "d" 0))
(def ^:private end-mark (.charCodeAt "e" 0))
(def ^:private length-mark (.charCodeAt ":" 0))
(def ^:private first-digit (.charCodeAt "0" 0))
(def ^:private last-digit (.charCodeAt "9" 0))

(def empty-decoder
  "A decoder that has read nothing."
  {:waiting [] :waiting-length 0 :needed 1 :open []})

(defn- read-token
  "The bencode piece at `offset` as [:value value after], [:open frame
  after], or [:close after], or [:short needed] when the buffer ends
  inside it and would need `needed` bytes to hold it."
  [^js buffer offset]
  (let [size (.-length buffer)
        mark (aget buffer offset)]
    (cond
      (= integer-mark mark)
      (let [end (.indexOf buffer end-mark offset)]
        (if (neg? end)
          [:short (inc size)]
          [:value (js/parseInt (.toString buffer "latin1" (inc offset) end) 10) (inc end)]))

      (= list-mark mark) [:open {:kind :list :items []} (inc offset)]
      (= dictionary-mark mark) [:open {:kind :dictionary :entries {} :key nil} (inc offset)]
      (= end-mark mark) [:close (inc offset)]

      (<= first-digit mark last-digit)
      (let [colon (.indexOf buffer length-mark offset)]
        (if (neg? colon)
          [:short (inc size)]
          (let [start (inc colon)
                end (+ start (js/parseInt (.toString buffer "latin1" offset colon) 10))]
            (if (<= end size)
              [:value (.toString buffer "utf8" start end) end]
              [:short end]))))

      :else (throw (ex-info "The bytes are not bencode" {:offset offset :byte mark})))))

(defn- with-value
  "[open finished] once `value` is whole, which joins the innermost open
  list or dictionary, as a key or its value, or else is finished."
  [open finished value]
  (if-let [frame (peek open)]
    [(conj (pop open)
           (cond
             (= :list (:kind frame)) (update frame :items conj value)
             (nil? (:key frame)) (assoc frame :key (keyword value))
             :else (-> frame (update :entries assoc (:key frame) value) (assoc :key nil))))
     finished]
    [open (conj finished value)]))

(defn decode-chunk
  "[values decoder] once `chunk` arrives. Bytes wait unread until they can
  finish the piece they continue, so a long reply is read once."
  [{:keys [waiting waiting-length needed open] :as decoder} ^js chunk]
  (let [waiting (conj waiting chunk)
        waiting-length (+ waiting-length (.-length chunk))]
    (cond
      (zero? (.-length chunk)) [[] decoder]
      (< waiting-length needed) [[] {:waiting waiting :waiting-length waiting-length :needed needed :open open}]
      :else
      (let [^js buffer (if (= 1 (count waiting)) chunk (js/Buffer.concat (into-array waiting)))
            size (.-length buffer)]
        (loop [offset 0 open open finished []]
          (if (= offset size)
            [finished (assoc empty-decoder :open open)]
            (let [[kind & found] (read-token buffer offset)]
              (case kind
                :value (let [[value after] found
                             [open finished] (with-value open finished value)]
                         (recur after open finished))
                :open (let [[frame after] found]
                        (recur after (conj open frame) finished))
                :close (let [[after] found
                             frame (peek open)]
                         (when-not frame
                           (throw (ex-info "The bytes are not bencode" {:offset offset})))
                         (let [[open finished] (with-value (pop open) finished
                                                           (if (= :list (:kind frame)) (:items frame) (:entries frame)))]
                           (recur after open finished)))
                :short (let [[short-needed] found
                             ;; A copy, so the bytes already read can go.
                             left (js/Buffer.from (.subarray buffer offset))]
                         [finished {:waiting [left] :waiting-length (.-length left)
                                    :needed (- short-needed offset) :open open}])))))))))

;; Connections

(def ^:private printed-value-limit
  "The most bytes of a printed value the server sends, through its print
  middleware's quota, so one huge result cannot flood the view."
  32768)

(defn done? [reply]
  (boolean (some #(= "done" %) (:status reply))))

(defn- deliver-replies!
  "Passes every reply `chunk` completes to the request awaiting it, or to
  `on-unclaimed`."
  [{:keys [state]} chunk on-unclaimed]
  (let [[replies decoder] (decode-chunk (:decoder @state) chunk)]
    (swap! state assoc :decoder decoder)
    (doseq [reply replies
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

(defn connect
  "Resolves to a connection with a session of its own, or rejects when
  none is cloned within `answer-milliseconds`. Requests waiting when it
  closes get a connection-closed reply."
  [{:keys [host port on-close on-unclaimed answer-milliseconds] :or {answer-milliseconds 5000}}]
  (js/Promise.
   (fn [resolve reject]
     (let [^js socket (.createConnection net #js {:host host :port port})
           state (atom {:decoder empty-decoder :awaiting {} :sent 0 :session nil})
           connection {:socket socket :state state :host host :port port}
           settled? (atom false)
           give-up! (fn [error]
                      (when-not @settled?
                        (reset! settled? true)
                        (.destroy socket)
                        (reject error)))
           timer (js/setTimeout #(give-up! (task/coded-error (str "No nREPL answer on port " port) "NREPL_SILENT"))
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
                (give-up! (task/coded-error "The connection closed before a session opened" "NREPL_SILENT")))))
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

(defn close! [{:keys [^js socket] :as connection}]
  (when-not (or (.-destroyed socket) (.-writableEnded socket))
    (request! connection {:op "close"} (fn [_reply] nil))
    (.end socket)))
