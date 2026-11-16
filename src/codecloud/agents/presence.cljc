(ns codecloud.agents.presence
  "Where agents work, as CodeCloud sees it, from what each agent read
  through the hub, what it said it works on, the pins it set for the
  person, and the changes to files credited to it. An agent is a session
  of the hub, or a subagent that names itself when it announces its work.
  Paths are absolute and times are milliseconds, passed in so the
  bookkeeping stays pure."
  (:require [clojure.string :as string]))

(def empty-presence
  "The presence before any agent arrives."
  {:agents {} :pins [] :pins-made 0})

(def read-lifetime
  "How long a read shows, in milliseconds: its band fades over four
  minutes."
  (* 4 60 1000))

(def change-lifetime
  "How long a change shows, in milliseconds: its glow fades over fifteen
  minutes."
  (* 15 60 1000))

(def unknown-agent-id
  "The agent a change goes to when no agent touched its file lately."
  "an-agent")

(def ^:private hues
  "The hues agents take in the order they arrive, in degrees, away from
  the reds that mark cycles and errors."
  [235 150 290 45 190 330 100 265])

(def ^:private most-kept
  "Reads and changes kept for each agent."
  24)

(defn file-name
  "The last part of a path, with either kind of slash."
  [path]
  (last (string/split (str path) #"[/\\]")))

(defn initials
  "Two letters for an agent's badge, the first letters of its name's first
  two words or the first two letters of a one-word name."
  [agent-name]
  (let [words (remove string/blank? (string/split (str agent-name) #"[\s\-_:()/]+"))]
    (cond
      (>= (count words) 2) (string/upper-case (str (first (first words)) (first (second words))))
      (seq words) (let [word (first words)]
                    (str (string/upper-case (subs word 0 1)) (subs word 1 (min 2 (count word)))))
      :else "?")))

(defn- with-agent
  "The presence with `agent`, {:id :name :client :parent}, active at
  `now`, added with the next hue when it is new."
  [presence {:keys [id] :as agent} now]
  (let [known (select-keys agent [:id :name :client :parent])]
    (if (get-in presence [:agents id])
      (update-in presence [:agents id] merge known {:last-at now})
      (assoc-in presence [:agents id]
                (merge {:hue (nth hues (mod (count (:agents presence)) (count hues)))
                        :initials (if (= unknown-agent-id id) "?" (initials (:name agent)))
                        :since now
                        :last-at now
                        :place nil
                        :doing nil
                        :intent nil
                        :reads []
                        :changes []
                        :touches {}}
                       known)))))

(defn- kept [entries now lifetime]
  (into [] (take-last most-kept (filter #(< (- now (:at %)) lifetime) entries))))

(defn read-by
  "The presence after `agent` read lines `start` to `end`, one-based, of
  `path`. Its badge moves to `line`, else the first line read; a
  whole-file read has no lines."
  [presence agent {:keys [path start end line doing]} now]
  (let [id (:id agent)
        with-doing (-> (with-agent presence agent now)
                       (assoc-in [:agents id :doing] doing))]
    (if-not path
      with-doing
      (-> with-doing
          (update-in [:agents id :reads] #(kept (conj % {:path path :start start :end end :at now}) now read-lifetime))
          (assoc-in [:agents id :place] (cond-> {:path path :line (or line start 1)}
                                          ;; A file read whole names no line.
                                          (nil? (or line start)) (assoc :whole? true)))
          (assoc-in [:agents id :touches path] now)))))

(defn announced
  "The presence after `agent` said at `now` that it works on `intent`, at
  line `line` of the file at `path` when it named one."
  [presence agent {:keys [path line intent]} now]
  (let [id (:id agent)]
    (cond-> (-> (with-agent presence agent now)
                (update-in [:agents id] assoc :intent intent :doing intent))
      path (-> (assoc-in [:agents id :place] {:path path :line (or line 1)})
               (assoc-in [:agents id :touches path] now)))))

(defn pinned
  "The presence after `agent` pinned `note` to line `line` of the file at
  `path` at `now`, for the person to click. The oldest pins go past a
  limit."
  [presence agent {:keys [path line note]} now]
  (let [most-pins 30
        number (inc (:pins-made presence))]
    (-> (with-agent presence agent now)
        (assoc :pins-made number)
        (update :pins #(vec (take-last most-pins (conj % {:id (str "pin-" number)
                                                          :agent-id (:id agent)
                                                          :path path
                                                          :line line
                                                          :note note
                                                          :at now})))))))

(defn unpinned [presence pin-id]
  (update presence :pins (fn [pins] (filterv #(not= pin-id (:id %)) pins))))

(defn credited
  "The id of the agent a change to the file at `path` at `now` goes to,
  the one that read or announced that file last within ten minutes, or
  nil."
  [presence path now]
  (let [credit-window (* 10 60 1000)]
    (some->> (:agents presence)
             (keep (fn [[id {:keys [touches]}]]
                     (when-let [touched-at (get touches path)]
                       (when (< (- now touched-at) credit-window) [id touched-at]))))
             seq
             (apply max-key second)
             first)))

(defn changed
  "The presence after a change to lines `start` to `end` of `path`,
  credited as a guess to the agent that touched the file last, or to an
  unnamed agent. Unknown lines cover the whole file."
  [presence {:keys [path start end added removed]} now]
  (let [credit (credited presence path now)
        agent (if credit
                (select-keys (get-in presence [:agents credit]) [:id :name :client :parent])
                {:id unknown-agent-id :name "An agent"})
        id (:id agent)]
    (-> (with-agent presence agent now)
        (update-in [:agents id :changes] #(kept (conj % {:path path :start start :end end :added added :removed removed
                                                         :at now :credited? (some? credit)})
                                                now change-lifetime))
        (update-in [:agents id] assoc
                   :place {:path path :line (or start 1)}
                   :doing (str "changing " (file-name path))))))

(defn expired
  "The presence at `now` without reads and changes past their lifetimes,
  and without the agents that left, whose sessions are not among
  `present-sessions` and who have nothing left to show and no pins."
  [presence present-sessions now]
  (let [pinned-agents (into #{} (map :agent-id) (:pins presence))]
    (update presence :agents
            (fn [agents]
              (into {}
                    (keep (fn [[id agent]]
                            (let [trimmed (-> agent
                                              (update :reads kept now read-lifetime)
                                              (update :changes kept now change-lifetime))]
                              (when (or (contains? present-sessions (or (:parent agent) id))
                                        (seq (:reads trimmed))
                                        (seq (:changes trimmed))
                                        (contains? pinned-agents id))
                                [id trimmed]))))
                    agents)))))

(defn summary
  "The agents and pins at `now` for the view and the coworkers tool,
  without bookkeeping and with only reads and changes still showing,
  latest active first."
  ([presence now] (summary presence now nil))
  ([presence now present-sessions]
   {:agents (->> (vals (:agents presence))
                 (sort-by (juxt (comp - :last-at) :id))
                 (mapv (fn [agent]
                         (cond-> (-> agent
                                     (dissoc :touches)
                                     (update :reads kept now read-lifetime)
                                     (update :changes kept now change-lifetime))
                           present-sessions (assoc :present? (contains? present-sessions (or (:parent agent) (:id agent))))))))
    :pins (:pins presence)}))

(defn line-change
  "How `new-lines` differ from `old-lines`, by the lines both share at the
  start and end, as {:start :end :added :removed} in one-based new lines,
  or nil."
  [old-lines new-lines]
  (when (not= old-lines new-lines)
    (let [old-lines (vec old-lines)
          new-lines (vec new-lines)
          old-count (count old-lines)
          new-count (count new-lines)
          shared-start (count (take-while true? (map = old-lines new-lines)))
          room (- (min old-count new-count) shared-start)
          shared-end (count (take-while true? (take room (map = (rseq old-lines) (rseq new-lines)))))
          added (- new-count shared-start shared-end)
          removed (- old-count shared-start shared-end)]
      {:start (inc shared-start)
       :end (max (inc shared-start) (+ shared-start added))
       :added added
       :removed removed})))
