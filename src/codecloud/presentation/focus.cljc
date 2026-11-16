(ns codecloud.presentation.focus
  "What the person is looking at in the cloud, reported to the host for
  agents that ask, from the snapshot shown and the lifted page or folder
  to the caret and selection of the page being edited, the pages on
  screen, and the filter.")

(defn report
  "The focus report of view state `state`, given `in-view`, the ids of
  the pages on screen, the largest first. Positions count from zero."
  [state in-view]
  (let [most-reported 40
        {:keys [path namespace-id]} (:editing state)
        {:keys [caret anchor]} (when path (get-in state [:buffers path]))]
    {:snapshot-id (get-in state [:snapshot :id])
     :snapshot-label (get-in state [:snapshot :label])
     :lifted (:focused-id state)
     :lifted-folder (:lifted-folder state)
     :editing (when (and path caret)
                {:namespace-id namespace-id
                 :path path
                 :line (first caret)
                 :column (second caret)
                 :anchor-line (first (or anchor caret))
                 :anchor-column (second (or anchor caret))})
     :in-view (vec (take most-reported in-view))
     :view-filter (:view-filter state)
     :visible-count (count (:visible-ids state))
     :arrangement (:arrangement state)}))
