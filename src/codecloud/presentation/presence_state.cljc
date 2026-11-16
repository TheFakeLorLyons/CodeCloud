(ns codecloud.presentation.presence-state
  "Where agents work, as the cloud shows it. The presence the host sends,
  with paths relative to the cloud's folder, is joined to the pages
  holding those paths, and the cloud keeps the agent the camera follows,
  the notices for changes to files open in the cloud, and what the
  Coworkers pane lists. Times are milliseconds, passed in so the
  bookkeeping stays pure."
  (:require [codecloud.agents.presence :as agent-presence]))

(def initial
  "The presence before the host says anything."
  {:agents [] :pins [] :following nil :pane-open? false :noticed-through nil})

(def read-lifetime
  "How long a read's band shows, in milliseconds."
  agent-presence/read-lifetime)

(def change-lifetime
  "How long a change's glow shows, in milliseconds."
  agent-presence/change-lifetime)

(defn page-index
  "Map from the path of every file of `snapshot` to the id of its page."
  [snapshot]
  (into {}
        (for [[page-id node] (:namespaces snapshot)
              {:keys [path]} (:files node)]
          [path page-id])))

(defn agent-named [presence agent-id]
  (some #(when (= agent-id (:id %)) %) (:agents presence)))

(defn received
  "The presence after the host's `message`, still following the agent the
  person chose while that agent stays."
  [presence {:keys [agents pins]}]
  (let [agent-ids (into #{} (map :id) agents)]
    (cond-> (assoc presence :agents (vec agents) :pins (vec pins))
      (not (contains? agent-ids (:following presence))) (assoc :following nil))))

(defn change-notices
  "[notices latest] for changes `agents` made after `noticed-through` to
  files among `open-paths`. Before anything was noticed every change
  counts as seen, so a view opening late raises no notice."
  [agents open-paths noticed-through]
  (let [changes (for [{agent-name :name :keys [changes]} agents
                      change changes]
                  (assoc change :agent-name agent-name))
        latest (reduce max (or noticed-through 0) (map :at changes))]
    [(if (nil? noticed-through)
       []
       (->> changes
            (filter #(and (> (:at %) noticed-through) (contains? open-paths (:path %))))
            (sort-by :at)
            (map (fn [{:keys [agent-name path]}] (str agent-name " changed " path ", which is open here.")))
            distinct
            vec))
     latest]))

(defn followed-place
  "Where the camera goes to keep up with the followed agent after the
  presence went from `before` to `after`, as {:namespace-id :path :line},
  with :whole? for a whole-file read, else nil."
  [before after page-of-path visible-ids]
  (let [following (:following after)
        before-place (:place (agent-named before following))
        after-place (:place (agent-named after following))
        page-id (get page-of-path (:path after-place))]
    (when (and following after-place (not= before-place after-place) (contains? visible-ids page-id))
      (cond-> {:namespace-id page-id :path (:path after-place) :line (:line after-place)}
        (:whole? after-place) (assoc :whole? true)))))

(defn place-location
  "A flight's destination for `place`, {:path :line}, as {:namespace-id
  :path :line}, or nil when no page holds its file."
  [page-of-path place]
  (when-let [page-id (get page-of-path (:path place))]
    {:namespace-id page-id :path (:path place) :line (max 1 (or (:line place) 1))}))

(defn activity
  "What agents did lately, newest first, at most `most-shown`, each a
  :read, :change, or :pin with the :agent that did it."
  [{:keys [agents pins]} most-shown]
  (let [by-id (into {} (map (juxt :id identity)) agents)]
    (->> (concat (for [agent agents
                       reading (:reads agent)
                       :when (:path reading)]
                   (assoc reading :kind :read :agent agent))
                 (for [agent agents
                       change (:changes agent)]
                   (assoc change :kind :change :agent agent))
                 (for [pin pins]
                   (assoc pin :kind :pin :agent (get by-id (:agent-id pin)))))
         (sort-by (comp - :at))
         (take most-shown)
         vec)))

(defn fade
  "The share of its strength a mark made at `at` keeps at `now` over
  `lifetime`, all of it when new, falling faster as it ages, and none at
  the end."
  [at now lifetime]
  (let [age (/ (max 0 (- now at)) lifetime)]
    (if (>= age 1) 0 (- 1 (* age age)))))

(defn page-changes
  "For each page with a change still showing at `now`, {:added :removed
  :whole? :agents}, :whole? when a change of unknown lines touched it."
  [{:keys [agents]} page-of-path now]
  (reduce (fn [totals [agent-name {:keys [path added removed at]}]]
            (let [page-id (get page-of-path path)]
              (if (and page-id (pos? (fade at now change-lifetime)))
                (update totals page-id (fn [{:keys [whole?] :as total}]
                                         (-> (merge {:added 0 :removed 0 :whole? false :agents []} total)
                                             (update :added + (or added 0))
                                             (update :removed + (or removed 0))
                                             (assoc :whole? (or whole? (nil? added)))
                                             (update :agents #(if (some #{agent-name} %) % (conj % agent-name))))))
                totals)))
          {}
          (for [{agent-name :name :keys [changes]} agents
                change changes]
            [agent-name change])))

(defn agent-color
  "The Oklch color of an agent of `hue` on `theme-name`'s sky, darker by
  day so it holds against light pages."
  [hue theme-name]
  (if (= :day theme-name) [0.58 0.16 hue] [0.78 0.13 hue]))
