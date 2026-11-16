(ns codecloud.vscode.session.command-messages
  "The host's side of the commands pages run for their files. A VS Code
  command runs with the file shown in the editor group holding the cloud,
  its caret where the cloud's caret was, and the cloud comes back to the
  front once the command has run. A command line runs in a terminal of
  its own in the cloud's folder."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.task :as task]
            [codecloud.presentation.page-commands :as page-commands]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.session.common :as common]))

(defn- run-in-editor!
  "Runs VS Code's `command` with `args`, if any, as from the editor of the
  file at `path`, then shows the cloud again."
  [{:keys [panel root-directory] :as session} {:keys [title command args path line column]}]
  (-> (api/show-text-document! (path/join root-directory path) line column (api/panel-view-column panel))
      (task/then (fn [_]
                   (if (some? args)
                     (api/execute-command command (clj->js args))
                     (api/execute-command command))))
      (task/then (fn [_] (api/reveal-panel! panel)))
      (task/recover (fn [error]
                      (api/reveal-panel! panel)
                      (common/post-notice! session (str (or title command) " did not run: " (task/error-text error)))))))

(defn- run-in-terminal!
  "Runs command line `shell` in a new terminal named `title`, showing it
  without taking the keys from the cloud."
  [{:keys [root-directory]} {:keys [title shell path line]}]
  (let [terminal (api/create-terminal title root-directory)]
    (api/send-to-terminal! terminal
                           (page-commands/command-line shell {:file (path/join root-directory path)
                                                              :relative-file path
                                                              :line line})
                           true)
    (api/show-terminal! terminal)))

(defmethod common/handle-message :view/run-page-command [session {:keys [shell] :as page-command}]
  (if shell
    (run-in-terminal! session page-command)
    (run-in-editor! session page-command)))

(defn- command-items
  "Every command extensions contribute, once each, labeled as VS Code's
  command palette labels them and in order of those labels."
  []
  (->> (api/contributed-commands)
       (reduce (fn [items {:keys [command title category]}]
                 (if (contains? items command)
                   items
                   (assoc items command {:label (cond
                                                  (and category title) (str category ": " title)
                                                  title title
                                                  :else command)
                                         :description command
                                         :value command})))
               {})
       vals
       (sort-by (comp string/lower-case :label))))

(defn pick-for-editor!
  "Offers every command extensions contribute and runs the one picked as
  it would run from the command palette, for the file in the editor when
  there is one."
  []
  (let [file-path (:path (api/active-editor-view))]
    (task/then (api/quick-pick (command-items) (if file-path
                                                 (str "A command to run for " (path/basename file-path))
                                                 "A command to run"))
               (fn [command]
                 (when command
                   (api/execute-command command))))))

(defmethod common/handle-message :view/pick-page-command [session {:keys [path] :as place}]
  (task/then (api/quick-pick (command-items) (str "A command to run for " (path/basename path)))
             (fn [command]
               (when command
                 (run-in-editor! session (assoc place :command command))))))
