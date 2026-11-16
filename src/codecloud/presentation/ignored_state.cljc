(ns codecloud.presentation.ignored-state
  "The Views pane's list of what the cloud leaves out, the folders and
  files that git, CodeCloud, or the cloud file ignore and the files of a
  kind CodeCloud does not read. The list stays closed until opened, asks the
  host for what a folder holds as the folder opens, and brings a chosen
  folder or file into the cloud, where it stays ignored and shows gray.
  The cloud remembers what was chosen."
  (:require [codecloud.model.ignored :as ignored]
            [codecloud.presentation.handling :refer [changed send-effect unchanged]]))

(defn- listing-request [folder]
  (send-effect :view/list-ignored {:path folder}))

(defn- toggle-ignored-open
  "Opens the list, asking again for what the cloud leaves out and for what
  each open folder holds, or closes it."
  [{:keys [ignored-open? open-ignored-folders] :as state}]
  (if ignored-open?
    (changed (assoc state :ignored-open? false))
    (apply changed (assoc state :ignored-open? true)
           (map listing-request (cons "" (sort open-ignored-folders))))))

(defn- toggle-ignored-folder-open
  "Opens folder `folder` in the list, asking for what it holds, or closes it."
  [state folder]
  (if (contains? (:open-ignored-folders state) folder)
    (changed (update state :open-ignored-folders disj folder))
    (changed (update state :open-ignored-folders conj folder) (listing-request folder))))

(defn- toggle-ignored-shown
  "Brings the folder or file at `path` into the cloud, or takes it out
  again, remembering the choice for the cloud."
  [state path]
  (let [shown (ignored/toggled (:shown-ignored state) path)]
    (changed (assoc state :shown-ignored shown)
             (send-effect :view/save-preference {:key :shown-ignored :value shown}))))

(defn- take-listing
  "The state with what the host listed inside `path`."
  [state {:keys [path entries more]}]
  (unchanged (assoc-in state [:ignored-listing path] {:entries (vec entries) :more (or more 0)})))

(def handlers
  {:toggle-ignored-open toggle-ignored-open
   :toggle-ignored-folder-open toggle-ignored-folder-open
   :toggle-ignored-shown toggle-ignored-shown})

(def message-handlers
  "Handlers of the host's listings of what the cloud leaves out, by type."
  {:host/ignored take-listing})
