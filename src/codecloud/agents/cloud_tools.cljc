(ns codecloud.agents.cloud-tools
  "The cloud's own tools for agents, answered from CodeCloud's analysis
  and from what the person is looking at. Each tool lives in a namespace
  under this one as {:spec :answer :paths-to-read :touched}. The context
  they answer from holds the cloud's folder as :root, the view's last
  report as :focus, the caret as :carets, the texts `paths-to-read`
  named as :texts, and for the coworkers tool the :presence and the
  :caller."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.cloud-overview :as cloud-overview]
            [codecloud.agents.cloud-tools.cloud-view :as cloud-view]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.agents.cloud-tools.page-details :as page-details]
            [codecloud.agents.cloud-tools.path-between :as path-between]
            [codecloud.agents.cloud-tools.reports :as reports]
            [codecloud.agents.cloud-tools.uses-of :as uses-of]
            [codecloud.agents.cloud-tools.viewer-focus :as viewer-focus]))

(def ^:private registry
  "Each tool, in the order the hub lists them. Its :answer, and its
  :paths-to-read and :touched when it has them, take the context and the
  call's arguments."
  [cloud-overview/tool page-details/tool uses-of/tool path-between/tool cloud-view/tool viewer-focus/tool reports/announce-work reports/point-out reports/coworkers])

(def tools
  "The cloud's tools as the hub lists them."
  (mapv :spec registry))

(def ^:private tools-by-name
  (into {} (map (juxt #(get-in % [:spec "name"]) identity)) registry))

(defn- announce-tool
  "announce_work as listed below :full, without a subagent's name, and at
  :brief without the line either."
  [reporting tool]
  (let [description (if (= :brief reporting)
                      (str "Shows the person the task the agent works on, as a badge on the page where it starts. "
                           "Call it once per task, as the task starts. A subagent leaves it to the agent "
                           "that started it.")
                      (str "Shows the person which file the agent works on and what it intends, as a badge on that "
                           "file's page in the cloud, even while the agent reads with its own tools. Call it on "
                           "starting with a file. A subagent leaves it to the agent that started it."))]
    (-> tool
        (assoc "description" description)
        (update-in ["inputSchema" "properties"] #(apply dissoc % (if (= :brief reporting) ["name" "line"] ["name"]))))))

(defn listed-tools
  "The cloud's tools as the hub lists them while agents report at level
  `reporting`. Quieter levels change announce_work, then withdraw
  point_out and coworkers, and at :off announce_work as well."
  [reporting]
  (let [reporting-tools #{"announce_work" "point_out" "coworkers"}]
    (into []
          (keep (fn [tool]
                  (let [tool-name (get tool "name")]
                    (cond
                      (or (= :full reporting) (not (contains? reporting-tools tool-name))) tool
                      (= :off reporting) nil
                      (= "announce_work" tool-name) (announce-tool reporting tool)
                      (= :brief reporting) nil
                      :else tool))))
          tools)))

(defn tool? [tool-name]
  (contains? tools-by-name tool-name))

(def presence-request
  "What a call of announce_work or point_out asks of the presence, as
  codecloud.agents.cloud-tools.reports/presence-request says."
  reports/presence-request)

(defn touched-places
  "What a call of tool `tool-name` read, as [{:path :start :end :doing}]
  with one-based lines; a page read whole has no lines."
  [{:keys [snapshot] :as context} tool-name arguments]
  (when snapshot
    (if-let [touched (:touched (tools-by-name tool-name))]
      (touched context (or arguments {}))
      [])))

(defn- folder-note
  "Where the snapshot's paths start, when that is not the first workspace
  folder, or nil."
  [{:keys [root workspace-roots]}]
  (when root
    (let [root (string/replace (common/forward-slashes root) #"/+$" "")
          workspace-root (some-> (first workspace-roots) common/forward-slashes (string/replace #"/+$" ""))
          inside (when workspace-root (common/within-folder workspace-root root))]
      (cond
        (= "" inside) nil
        inside (str "Paths are relative to " inside ", the cloud's folder within the workspace.")
        :else (str "Paths are relative to " root ", the cloud's folder.")))))

(defn paths-to-read
  "The files, relative to the cloud's folder, whose text the answer to
  `tool-name` with `arguments` reads."
  [{:keys [snapshot] :as context} tool-name arguments]
  (if-let [paths (when snapshot (:paths-to-read (tools-by-name tool-name)))]
    (paths context (or arguments {}))
    []))

(defn answer
  "The answer to a call of tool `tool-name`, as {:text} or {:error};
  announce_work and point_out also give an :effect."
  [{:keys [snapshot] :as context} tool-name arguments]
  (let [arguments (or arguments {})
        answered (cond
                   (not (tool? tool-name)) {:error (str "The cloud has no tool named " tool-name ".")}
                   (nil? snapshot) {:error "The cloud has not read the codebase yet. Ask again in a moment."}
                   :else ((:answer (tools-by-name tool-name)) context arguments))
        note (folder-note context)]
    (if (and (:text answered) note)
      (update answered :text str "\n\n" note)
      answered)))
