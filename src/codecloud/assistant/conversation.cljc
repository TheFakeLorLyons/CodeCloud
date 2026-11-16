(ns codecloud.assistant.conversation
  "One conversation with CodeCloud's assistant, kept pure, holding the
  turns sent to the model, the entries the cloud's pane shows, the
  assistant's system prompt, and the tools it gets from the hub. A turn
  is {:role :user :text}, {:role :assistant :text :tool-calls}, or {:role
  :tool :results}. An entry is {:id :version :kind}, its :version
  counting its changes so the pane redraws only what changed."
  (:require [clojure.string :as string]))

(def most-rounds
  "Rounds of tool calls one question may take before the assistant stops
  to let the person decide whether to go on."
  24)

(def most-result-characters
  "The longest tool result passed to the model, in characters; longer
  results are cut, saying so."
  20000)

(def reply-token-cap
  "The longest reply asked for, in tokens, where an API asks for a cap."
  4096)

(def empty-conversation
  "A conversation before the first question."
  {:turns [] :entries [] :next-id 1})

(defn system-prompt
  "The assistant's instructions, on where it works, how to read the
  codebase through the hub's tools, and how to answer."
  [hub-instructions cloud-name pins?]
  (string/join
   "\n\n"
   (remove string/blank?
           [(str "You are the assistant built into CodeCloud, the VS Code extension that shows the codebase "
                 (when (seq cloud-name) (str "called " cloud-name " "))
                 "as a cloud of pages, one for each namespace or file. The person asks about this codebase from "
                 "the cloud, and you answer from what the tools find, looking before answering.")
            hub-instructions
            (str "When the question says this or here, start from viewer_focus, which says what the person is "
                 "looking at. Name places as path:line, counting lines from 1, which the person can click to fly "
                 "there" (when pins? ", and use point_out for a place the person should see") ". You read and "
                 "explain; you cannot change files, so say what to change and where. Answer briefly and concretely, "
                 "in Markdown.")])))

(defn tool-definitions
  "The tools the hub lists, as the assistant offers them to a model:
  {:name :title :description :schema}."
  [hub-tools]
  (mapv (fn [tool]
          {:name (get tool "name")
           :title (get tool "title")
           :description (get tool "description")
           :schema (or (get tool "inputSchema") {"type" "object" "properties" {}})})
        hub-tools))

(defn tool-target
  "What a tool call is about, for its entry, naming the file, page, or
  name the call names with its line, or with the line counted from 0 that
  wavescope takes as its center, or nil when it names none."
  [input]
  (let [target (some #(let [value (get input %)] (when (and (string? value) (seq value)) value))
                     ["file" "page" "name" "from" "folder" "directory" "intent"])
        line (get input "line")
        center (get input "center")]
    (when target
      (str target
           (when (seq (get input "to")) (str " to " (get input "to")))
           (cond
             (number? line) (str ":" line)
             (number? center) (str ":" (inc center))
             :else nil)))))

(defn clipped-result
  "A tool result's text as the model receives it, cut at
  `most-result-characters`."
  [text]
  (if (> (count text) most-result-characters)
    (str (subs text 0 most-result-characters) "\n\n[The result went on; ask for less to see the rest.]")
    text))

;; Entries

(defn- with-entry [{:keys [next-id] :as conversation} entry]
  [(-> conversation
       (update :entries conj (assoc entry :id next-id :version 1))
       (assoc :next-id (inc next-id)))
   next-id])

(defn- revise-entry [conversation id change]
  (update conversation :entries
          (fn [entries]
            (mapv #(if (= id (:id %)) (-> % change (update :version inc)) %) entries))))

(defn asked [conversation text]
  (-> conversation
      (update :turns conj {:role :user :text text})
      (with-entry {:kind :question :text text})
      first))

(defn reply-begun
  "[conversation id] with an empty answer entry the reply streams into."
  [conversation]
  (with-entry conversation {:kind :answer :text "" :streaming? true}))

(defn reply-streamed
  "The conversation with answer entry `id` showing `text`, the reply so
  far."
  [conversation id text]
  (revise-entry conversation id #(assoc % :text text)))

(defn replied
  "The conversation after the reply streamed into entry `id` ended as
  `reply`, {:text :tool-calls}, each call with its parsed :input. The
  entry goes when the reply said nothing."
  [conversation id {:keys [text tool-calls]}]
  (let [ended (if (string/blank? text)
                (update conversation :entries (fn [entries] (filterv #(not= id (:id %)) entries)))
                (revise-entry conversation id #(assoc % :text text :streaming? false)))]
    (cond-> ended
      (or (seq text) (seq tool-calls)) (update :turns conj {:role :assistant :text text :tool-calls (vec tool-calls)}))))

(defn tool-begun
  "[conversation id] with a running tool entry for `call`, titled by
  `title`, what the tool's listing calls it."
  [conversation {tool-name :name :keys [input]} title]
  (with-entry conversation {:kind :tool :name tool-name :title (or title tool-name) :target (tool-target input) :status :running}))

(defn tool-ended [conversation id {:keys [text error?]}]
  (revise-entry conversation id #(assoc % :status (if error? :failed :done) :result text)))

(defn tools-answered
  "The conversation after the tools answered `results`, {:id :name :text
  :error?}, in the order the model called them."
  [conversation results]
  (update conversation :turns conj {:role :tool :results (vec results)}))

(defn noted [conversation text]
  (first (with-entry conversation {:kind :notice :text text})))

(defn shown
  "The entries as the pane receives them, a tool's result cut short."
  [{:keys [entries]}]
  (let [most-shown-characters 4000]
    (mapv (fn [{:keys [result] :as entry}]
            (if (and (string? result) (> (count result) most-shown-characters))
              (assoc entry :result (str (subs result 0 most-shown-characters) "\n…"))
              entry))
          entries)))
