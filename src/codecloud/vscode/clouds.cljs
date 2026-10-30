(ns codecloud.vscode.clouds
  "Finds the cloud covering a file, creates a cloud where none exists,
  and opens a cloud where the viewer left the editor."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.model.config :as config]
            [codecloud.model.workspace :as workspace]
            [codecloud.vscode.api :as api]))

(def view-type
  "The custom editor that shows *.cld files as clouds."
  "codecloud.cloud")

(defn- forward-slashes [file-path]
  (string/replace file-path "\\" "/"))

(defn comparison-path
  "A path written the way the workspace rules compare paths, with forward
  slashes and, where the file system ignores case, one letter case."
  [file-path]
  (let [forward (forward-slashes file-path)]
    (if (contains? #{"win32" "darwin"} (.-platform js/process)) (string/lower-case forward) forward)))

;; The workspace's cloud files

(defn- refresh-index! [index]
  (task/then (api/find-files "**/*.cld" "**/node_modules/**")
             (fn [paths] (reset! (:paths index) (vec (sort paths))))))

(defn create-index!
  "Keeps the list of cloud files in the workspace current. Returns
  {:paths (atom [path]) :ready :disposable}, where :ready resolves once
  the first search of the workspace is done."
  []
  (let [index {:paths (atom [])}
        pending (atom nil)
        settle-milliseconds 300
        schedule! (fn [_]
                    (js/clearTimeout @pending)
                    (reset! pending (js/setTimeout #(refresh-index! index) settle-milliseconds)))]
    (assoc index
           :ready (task/recover (refresh-index! index) (fn [_error] nil))
           :disposable (api/watch-workspace! "**/*.cld" schedule!))))

(defn after-first-search
  "Runs `action` once the index has searched the workspace, so a command
  that wakes the extension finds the clouds already there."
  [index action]
  (task/then (:ready index) (fn [_] (action))))

(defn cloud-file-for
  "The cloud file covering `file-path`, or nil."
  [index file-path]
  (let [by-comparison (into {} (map (juxt comparison-path identity)) @(:paths index))]
    (some-> (workspace/nearest-cloud-file (keys by-comparison) (comparison-path file-path) config/cloud-file-name)
            by-comparison)))

;; Creating a cloud

(defn- file-exists? [file-path]
  (-> (.access (.-promises fs) file-path)
      (task/then (constantly true))
      (task/recover (constantly false))))

(defn- project-directories
  "Resolves to those of `directories` that hold a project file such as
  deps.edn."
  [directories]
  (task/then (task/all (for [directory directories]
                         (task/then (task/all (for [marker workspace/project-files]
                                                (file-exists? (str directory "/" marker))))
                                    (fn [found] (when (some true? found) directory)))))
             (fn [found] (set (remove nil? found)))))

(defn suggested-home
  "Resolves to the folder where a cloud for `file-path` belongs: the
  nearest enclosing project folder within the workspace folder."
  [file-path]
  (let [top (forward-slashes (or (api/workspace-folder-path-of file-path) (path/dirname file-path)))
        target (forward-slashes file-path)]
    (task/then (project-directories (workspace/ancestor-directories top target))
               (fn [project-folders] (workspace/cloud-home top target project-folders)))))

(defn ensure-cloud-file!
  "Resolves to the path of the cloud file in `directory`, writing the
  starting template when there is none yet."
  [directory]
  (let [cloud-file (path/join directory config/cloud-file-name)]
    (task/then (file-exists? cloud-file)
               (fn [present?]
                 (if present?
                   cloud-file
                   (task/then (files/write-text cloud-file config/cloud-file-template)
                              (constantly cloud-file)))))))

;; Opening a cloud

(defn- relative-arrival
  "An arrival at an absolute file path, made relative to the cloud's
  folder, or nil when the file lies outside it."
  [cloud-file {:keys [file-path] :as arrival}]
  (when (and arrival file-path)
    (let [relative (forward-slashes (path/relative (path/dirname cloud-file) file-path))]
      (when-not (or (string/starts-with? relative "..") (path/isAbsolute relative))
        (-> arrival (dissoc :file-path) (assoc :path relative))))))

(defn open-cloud!
  "Shows `cloud-file` in `view-column`, starting on the line of `arrival`
  {:file-path :line :text-scale} when given. A session opening later
  takes it from the pending arrivals."
  [{:keys [sessions pending-arrivals deliver-arrival!]} cloud-file arrival view-column]
  (let [cloud-key (comparison-path cloud-file)
        relative (relative-arrival cloud-file arrival)]
    (when relative
      (swap! pending-arrivals assoc cloud-key relative))
    (task/then (api/open-with! (api/file-uri cloud-file) view-type view-column)
               (fn [_]
                 (when-let [pending (get @pending-arrivals cloud-key)]
                   (when-let [open-sessions (seq (get @sessions cloud-key))]
                     (swap! pending-arrivals dissoc cloud-key)
                     (doseq [session open-sessions]
                       (deliver-arrival! session pending))))))))

(defn create-and-open!
  "Creates a cloud file in `directory` when it has none, then opens it.
  The index learns of the new file at once, ahead of the file watcher."
  [{:keys [index] :as host} directory arrival view-column]
  (task/then (ensure-cloud-file! directory)
             (fn [cloud-file]
               (swap! (:paths index) #(vec (sort (distinct (conj % cloud-file)))))
               (task/then (open-cloud! host cloud-file arrival view-column)
                          (constantly cloud-file)))))

(defn- ask-to-create!
  "Asks whether to create a cloud in `home`, or in a folder the viewer
  picks, then creates and opens it."
  [host question home arrival view-column]
  (task/then (api/info-message question "Create Cloud" "Choose Folder")
             (fn [choice]
               (case choice
                 "Create Cloud" (create-and-open! host home arrival view-column)
                 "Choose Folder" (task/then (api/pick-folder home "Create Cloud Here")
                                            (fn [chosen]
                                              (when chosen
                                                (create-and-open! host chosen arrival view-column))))
                 nil))))

(defn offer-new-cloud!
  "Offers to create a cloud for `file-path` in its nearest project
  folder."
  [host file-path arrival view-column]
  (task/then (suggested-home file-path)
             (fn [home]
               (ask-to-create! host
                               (str "No cloud covers this file yet. Create " config/cloud-file-name
                                    " in " (path/basename home) "?")
                               home arrival view-column))))

(defn choose-cloud!
  "Opens a cloud when no editor says which: the only one, one picked from
  several, or a new one in a workspace folder."
  [{:keys [index] :as host} view-column]
  (let [cloud-files @(:paths index)]
    (case (count cloud-files)
      0 (let [folders (api/workspace-folder-paths)]
          (if (empty? folders)
            (api/info-message "Open a folder first; CodeCloud draws the folder's code.")
            (task/then (if (= 1 (count folders))
                         (task/resolved (first folders))
                         (api/quick-pick (map (fn [folder] {:label (path/basename folder) :description folder :value folder})
                                              folders)
                                         "Folder to draw as a cloud"))
                       (fn [folder]
                         (when folder
                           (ask-to-create! host
                                           (str "This workspace has no cloud yet. Create " config/cloud-file-name
                                                " in " (path/basename folder) "?")
                                           folder nil view-column))))))
      1 (open-cloud! host (first cloud-files) nil view-column)
      (task/then (api/quick-pick (map (fn [cloud-file]
                                        {:label (workspace/cloud-name (forward-slashes cloud-file) config/cloud-file-name)
                                         :description cloud-file
                                         :value cloud-file})
                                      cloud-files)
                                 "Cloud to open")
                 (fn [cloud-file] (when cloud-file (open-cloud! host cloud-file nil view-column)))))))
