(ns codecloud.assistant.runner
  "CodeCloud's assistant at work, asking a provider for its models and its
  replies, streamed over HTTP, and running a question through the model
  and the hub's tools until the model answers. Nothing here knows VS
  Code; codecloud.vscode.assistant-host supplies the provider, the key, and
  the hub."
  (:require [codecloud.assistant.conversation :as conversation]
            [codecloud.assistant.providers :as providers]
            [codecloud.io.task :as task]))

(defn- parsed-json [text]
  (try
    (js->clj (js/JSON.parse text))
    (catch :default _error nil)))

(defn- fetch!
  "Resolves to the response to `request`, {:method :url :headers :body},
  rejecting with the provider's own words when it answers with an
  error, or with a plain account when it cannot be reached."
  [{:keys [method url headers body]} signal]
  (-> (js/fetch url (clj->js (cond-> {:method method :headers headers}
                               body (assoc :body (js/JSON.stringify (clj->js body)))
                               signal (assoc :signal signal))))
      (task/recover (fn [^js error]
                      (task/rejected (if (= "AbortError" (.-name error))
                                       error
                                       (js/Error. (str "CodeCloud could not reach " url ": "
                                                       (or (some-> error .-cause .-message) (.-message error))))))))
      (task/then (fn [^js response]
                   (if (.-ok response)
                     response
                     (task/then (.text response)
                                (fn [text]
                                  (task/rejected (js/Error. (providers/error-message (.-status response) (parsed-json text)))))))))))

(defn list-models!
  "Resolves to the models `provider` offers for `api-key`, as
  codecloud.assistant.providers/model-list gives them."
  [provider api-key]
  (-> (fetch! (providers/models-request provider api-key) nil)
      (task/then (fn [^js response] (.text response)))
      (task/then (fn [text] (providers/model-list provider (parsed-json text))))))

(defn- read-body!
  "Resolves once `response`'s body ends, handing each chunk of its text
  to `on-text`."
  [^js response on-text]
  (let [^js reader (.getReader (.-body response))
        decoder (js/TextDecoder. "utf-8")]
    (letfn [(step []
              (task/then (.read reader)
                         (fn [^js chunk]
                           (if (.-done chunk)
                             (on-text (.decode decoder))
                             (do (on-text (.decode decoder (.-value chunk) #js {:stream true}))
                                 (step))))))]
      (step))))

(defn stream-reply!
  "Resolves to the reply `provider` streams for `request`, handing
  `on-progress` the reply so far after each chunk. A server that answers
  whole is read whole."
  [{:keys [kind]} request signal on-progress]
  (task/then (fetch! request signal)
             (fn [^js response]
               (if-not (re-find #"event-stream" (or (.get (.-headers response) "content-type") ""))
                 (task/then (.text response) #(providers/whole-reply kind (parsed-json %)))
                 (let [fold (case kind :anthropic providers/anthropic-event :openai-chat providers/openai-chunk)
                       state (atom {:waiting "" :reply providers/empty-reply})]
                   (task/then (read-body! response
                                          (fn [text]
                                            (let [[events waiting] (providers/sse-events (str (:waiting @state) text))]
                                              (swap! state assoc :waiting waiting)
                                              (doseq [{:keys [data]} events
                                                      :when (not= "[DONE]" data)
                                                      :let [parsed (parsed-json data)]
                                                      :when (map? parsed)]
                                                (swap! state update :reply fold parsed))
                                              (when (seq events)
                                                (on-progress (:reply @state))))))
                              (fn [_] (providers/completed (:reply @state)))))))))

(defn- with-input [{:keys [input input-json] :as call}]
  (let [parsed (or input (parsed-json (or input-json "{}")) {})]
    (assoc call
           :input (if (map? parsed) parsed {})
           :input-json (or input-json (js/JSON.stringify (clj->js parsed))))))

(defn- result-text [result]
  (let [texts (keep #(when (= "text" (get % "type")) (get % "text")) (get result "content"))]
    (if (seq texts) (apply str (interpose "\n" texts)) "The tool answered with nothing.")))

(defn answer!
  "Runs the conversation through the model and the hub's tools until the
  model answers without a tool call, the person stops it, or most-rounds
  pass."
  [{:keys [conversation on-change ask! run-tool! titles stopped?]}]
  (let [revise! (fn [change & arguments]
                  (on-change (apply swap! conversation change arguments)))
        begin! (fn [begin & arguments]
                 (let [[revised id] (apply begin @conversation arguments)]
                   (reset! conversation revised)
                   (on-change revised)
                   id))]
    (letfn [(run-calls! [calls]
              (task/then
               (reduce (fn [so-far {:keys [id name input] :as call}]
                         (task/then so-far
                                    (fn [results]
                                      (if (stopped?)
                                        (conj results {:id id :name name :text "The person stopped the question before this ran." :error? true})
                                        (let [entry-id (begin! conversation/tool-begun call (get titles name))]
                                          (-> (run-tool! name input)
                                              (task/then (fn [result] {:text (result-text result) :error? (true? (get result "isError"))}))
                                              (task/recover (fn [^js error] {:text (str "The tool failed: " (.-message error)) :error? true}))
                                              (task/then (fn [{:keys [text error?] :as outcome}]
                                                           (revise! conversation/tool-ended entry-id outcome)
                                                           (conj results {:id id :name name :text (conversation/clipped-result text)
                                                                          :error? error?})))))))))
                       (task/resolved [])
                       calls)
               (fn [results] (revise! conversation/tools-answered results))))
            (round! [round]
              (cond
                (stopped?) (revise! conversation/noted "Stopped.")
                (> round conversation/most-rounds)
                (revise! conversation/noted (str "The assistant stopped after " conversation/most-rounds
                                                 " rounds of tools. Ask again to let it go on."))
                :else
                (let [entry-id (begin! conversation/reply-begun)
                      streamed (atom "")]
                  (-> (ask! (:turns @conversation)
                            (fn [reply]
                              (reset! streamed (:text reply))
                              (revise! conversation/reply-streamed entry-id (:text reply))))
                      (task/then (fn [reply]
                                   (let [calls (mapv with-input (:tool-calls reply))]
                                     (revise! conversation/replied entry-id (assoc reply :tool-calls calls))
                                     (cond
                                       (:error reply) (revise! conversation/noted (:error reply))
                                       (empty? calls) nil
                                       :else (task/then (run-calls! calls) (fn [_] (round! (inc round))))))))
                      (task/recover (fn [^js error]
                                      (revise! conversation/replied entry-id {:text @streamed :tool-calls []})
                                      (revise! conversation/noted (if (or (stopped?) (= "AbortError" (.-name error)))
                                                                    "Stopped."
                                                                    (.-message error)))))))))]
      (round! 1))))

(defn run-question!
  "Asks `question` in the conversation held by the options' :conversation,
  then runs it as answer! does with the same options."
  [{:keys [conversation on-change] :as options} question]
  (on-change (swap! conversation conversation/asked question))
  (answer! options))
