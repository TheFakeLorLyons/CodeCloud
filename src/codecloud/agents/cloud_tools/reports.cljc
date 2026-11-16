(ns codecloud.agents.cloud-tools.reports
  "The tools agents report their work with, announce_work and point_out,
  and the coworkers tool, which lists what every agent reported."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.agents.presence :as presence]
            [codecloud.counting :refer [ago counted]]
            [codecloud.languages.catalog :as catalog]
            [codecloud.presentation.page :as page]))

;; The presence tools, announce_work, point_out, and coworkers

(defn- find-file
  "{:path :page-id :line-count} for the file `given` names, or {:error}. A
  bare name is a new path only when a language claims it, so a mistyped
  namespace is no new file."
  [{:keys [snapshot] :as context} given]
  (let [held (into {}
                   (for [[page-id node] (:namespaces snapshot)
                         {:keys [path line-count]} (:files node)]
                     [path {:path path :page-id page-id :line-count line-count}]))
        candidates (common/cloud-paths context given)
        file-like? (or (string/includes? (common/forward-slashes given) "/") (some? (catalog/language-by-name given)))]
    (if (string/blank? given)
      {:error "Name a file, or a namespace such as app.core."}
      (or (some held candidates)
          (let [{:keys [id error]} (common/find-page context given)]
            (cond
              id (get held (:path (page/primary-file (get-in snapshot [:namespaces id]))))
              (and file-like? (seq candidates)) {:path (first candidates)}
              :else {:error error}))))))

(defn- without-final-stop [text]
  (string/replace text #"[.\s]+$" ""))

(defn presence-request
  "What announce_work or point_out asks the cloud to show, as {:effect
  :text} or {:error}. Work may be announced on a file no page holds yet;
  a pin needs a page."
  [context tool-name arguments]
  (let [intent (without-final-stop (common/text-argument arguments "intent"))
        note (common/text-argument arguments "note")
        named (common/text-argument arguments "name")
        line (let [given (get arguments "line")] (when (number? given) (max 1 (int given))))
        target (when (seq (common/text-argument arguments "file")) (find-file context (common/text-argument arguments "file")))
        where (when (:path target) (str (:path target) (when line (str ", line " line))))]
    (case tool-name
      "announce_work"
      (cond
        (string/blank? intent) {:error "Say what the work is, as intent."}
        (:error target) target
        :else {:effect {:kind :announce :path (:path target) :line line :intent intent :name (not-empty named)}
               :text (str "The person sees " (if (seq named) named "this agent") " working on " intent
                          (when where (str ", at " where))
                          (when (and target (nil? (:page-id target)))
                            (str ". " (:path target) " has no page in the cloud yet, so the badge waits in the Coworkers pane"))
                          ".")})

      "point_out"
      (cond
        (nil? target) {:error "Name the file to point to."}
        (:error target) target
        (nil? (:page-id target)) {:error (str (:path target) " has no page in the cloud, so the person could not see a pin there.")}
        (nil? line) {:error "Name the line to point to, counting from 1."}
        (and (:line-count target) (> line (:line-count target)))
        {:error (str (:path target) " has " (counted (:line-count target) "line") ", so it has no line " line ".")}
        (string/blank? note) {:error "Write the note the person should see."}
        :else {:effect {:kind :pin :path (:path target) :line line :note note}
               :text (str "Pinned the note to " where ". The person clicks the pin to fly there.")})

      {:error (str tool-name " shows nothing in the cloud.")})))

(defn- change-text [context {:keys [path start end added removed]}]
  (str (common/cloud-relative context path)
       (when start (str ", " (common/line-span start end)))
       (when (and added removed) (str ", +" added " −" removed))))

(defn- agent-row
  "One agent's line for the coworkers tool, saying where it is, what it
  does and intends, and what it read and changed last."
  [{:keys [now caller] :as context} names {agent-name :name :keys [id parent place doing intent reads changes present?]}]
  (let [last-read (peek reads)
        last-change (peek changes)]
    (str "  " (if (= presence/unknown-agent-id id) "An agent CodeCloud could not name" agent-name)
         (cond
           (= caller id) ", this session"
           (and parent (= caller parent)) ", a subagent of this session"
           parent (str ", a subagent of " (get names parent "another agent")))
         (when (false? present?) ", no longer connected")
         ": "
         (string/join "; " (remove nil? [(when place (str "at " (common/cloud-relative context (:path place)) " line " (:line place)))
                                         (when (and intent (not= intent doing)) (str "intends " intent))
                                         doing
                                         (when last-read (str "read " (common/cloud-relative context (:path last-read))
                                                              (when (:start last-read) (str ", " (common/line-span (:start last-read) (:end last-read))))
                                                              ", " (ago now (:at last-read))))
                                         (when last-change (str "changed " (change-text context last-change) ", " (ago now (:at last-change))
                                                                (when (:credited? last-change) ", by CodeCloud's guess")))]))
         ".")))

(defn- coworkers-answer [{:keys [presence caller] :as context} _arguments]
  (let [{:keys [agents pins]} presence
        names (into {} (map (juxt :id :name)) agents)
        others (remove #(or (= caller (:id %)) (= caller (:parent %))) agents)
        most-pins 10]
    {:text (string/join
            "\n"
            (concat
             (if (seq others)
               (cons "Agents working here, the latest active first:" (map #(agent-row context names %) agents))
               ["No other agent works here now."])
             (when (seq pins)
               (cons (str "Pins waiting for the person to click: " (count pins) ".")
                     (for [{:keys [agent-id path line note]} (take most-pins pins)]
                       (str "  " (common/cloud-relative context path) ", line " line ", from " (get names agent-id "an agent") ": " note))))))}))

(def announce-work
  {:spec {"name" "announce_work"
          "title" "Say where the work is"
          "description" (str "Shows the person which file the agent works on and what it intends, as a badge on that file's page "
                             "in the cloud, even while the agent reads with its own tools. Call it on starting with a file. A "
                             "subagent sharing its parent's connection passes its own name and appears on its own.")
          "inputSchema" {"type" "object"
                         "properties" {"intent" {"type" "string" "description" "What the agent is doing, in a few words"}
                                       "file" (assoc common/page-property "description"
                                                     "The file or namespace it works on, absolute or relative to the workspace folder")
                                       "line" {"type" "integer" "minimum" 1 "description" "The line it works at, counting from 1"}
                                       "name" {"type" "string" "description" "A subagent's own name"}}
                         "required" ["intent"]}
          "annotations" common/marking}
   :answer (fn [context arguments] (presence-request context "announce_work" arguments))})

(def point-out
  {:spec {"name" "point_out"
          "title" "Point the person to a line"
          "description" (str "Pins a short note to a line for the person, who clicks the pin in the cloud to fly there. The "
                             "person's camera never moves by itself.")
          "inputSchema" {"type" "object"
                         "properties" {"file" (assoc common/page-property "description"
                                                     "The file or namespace, absolute or relative to the workspace folder")
                                       "line" {"type" "integer" "minimum" 1 "description" "The line, counting from 1"}
                                       "note" {"type" "string" "description" "What the person should see there, in a sentence"}}
                         "required" ["file" "line" "note"]}
          "annotations" common/marking}
   :answer (fn [context arguments] (presence-request context "point_out" arguments))})

(def coworkers
  {:spec {"name" "coworkers"
          "title" "Who else works here"
          "description" (str "Every agent working in this codebase now, with the file it is on, what it read and changed last, "
                             "and what it intends, so work can keep clear of a file another agent is changing.")
          "inputSchema" {"type" "object" "properties" {}}
          "annotations" common/read-only}
   :answer coworkers-answer
   :touched (fn [_context _arguments] [{:doing "checking who else works here"}])})
