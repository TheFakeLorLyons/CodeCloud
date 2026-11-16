(ns codecloud.presentation.assistant-state
  "The assistant pane's state, holding whether it shows and the
  conversation as the host last sent it, with the model chosen, whether
  one is, and whether the agent hub gives the assistant its tools.
  Handlers take the whole view state and return {:state :effects}, as
  those of codecloud.presentation.ui-state do. Also finds the places an
  answer names among the cloud's files."
  (:require [clojure.string :as string]
            [codecloud.agents.presence :as agent-presence]
            [codecloud.presentation.handling :refer [changed send-effect unchanged]]))

(def initial
  "The pane before the host says anything."
  {:open? false :entries [] :running? false :model nil :ready? false :hub? false})

(defn ask
  "Sends the person's question `text` to the host, which shows it in the
  conversation as the assistant takes it. Nothing goes while a question
  runs, and with no model chosen the host's model picker opens instead."
  [state text]
  (let [question (string/trim (str text))
        {:keys [running? ready?]} (:assistant state)]
    (cond
      (or (string/blank? question) running?) (unchanged state)
      (not ready?) (changed state (send-effect :view/assistant-model {}))
      :else (changed (assoc-in state [:assistant :running?] true)
                     (send-effect :view/assistant-ask {:text question})))))

(defn take-message
  "The state after the host sent the conversation, as :host/assistant
  describes it."
  [state {:keys [entries running? model ready? hub?]}]
  (update state :assistant assoc
          :entries (vec entries)
          :running? (boolean running?)
          :model model
          :ready? (boolean ready?)
          :hub? (boolean hub?)))

(def handlers
  "Each assistant event's handler, by the event's first element."
  {:assistant-ask ask
   :assistant-stop (fn [state] (changed state (send-effect :view/assistant-stop {})))
   :assistant-new (fn [state]
                    (changed (update state :assistant assoc :entries [] :running? false)
                             (send-effect :view/assistant-new {})))
   :assistant-model (fn [state] (changed state (send-effect :view/assistant-model {})))})

(def message-handlers
  "Each assistant message's handler, changing only the state, by the host
  message's :type."
  {:host/assistant take-message})

;; Places an answer names

(def ^:private reference-parts
  "A reference to a place, a path followed by a line and an end line after
  a dash, both optional."
  #"^(?:\./)?(.+?)(?::(\d+)(?:[-–](\d+))?)?$")

(defn place-finder
  "A function giving {:path :line} for a reference such as src/core.clj:12
  or a bare path, or nil when it matches none or several of `paths`."
  [paths]
  (let [known (set paths)
        by-file-name (group-by agent-presence/file-name paths)]
    (fn [reference]
      (when-let [[_ path line] (re-matches reference-parts (str reference))]
        (let [candidates (filter #(or (string/ends-with? % (str "/" path)) (string/ends-with? path (str "/" %)))
                                 (get by-file-name (agent-presence/file-name path)))
              found (cond
                      (contains? known path) path
                      (= 1 (count candidates)) (first candidates)
                      :else nil)]
          (when found
            {:path found
             :line (when line #?(:clj (Long/parseLong line) :cljs (js/parseInt line 10)))}))))))
