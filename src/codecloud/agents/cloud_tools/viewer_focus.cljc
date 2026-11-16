(ns codecloud.agents.cloud-tools.viewer-focus
  "The viewer_focus tool, where the person's caret is and what shows
  on their screen."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.counting :refer [ago counted]]))

;; viewer_focus

(defn- caret-path
  "The path, relative to the cloud's folder, of the file holding `caret`,
  or nil when it lies outside the cloud."
  [{:keys [root] :as context} caret]
  (when (and root (:file caret))
    (first (common/cloud-paths (assoc context :workspace-roots []) (:file caret)))))

(defn- caret-lines
  "Where `caret` is, in words, with the definition around it and the code
  under it or the lines it selects."
  [{:keys [snapshot texts now] :as context} caret]
  (let [most-selected-lines 30
        longest-code 200
        relative (caret-path context caret)
        files (common/file-index snapshot)
        page-id (get-in files [relative :page-id])
        node (get-in snapshot [:namespaces page-id])
        line (inc (:line caret 0))
        column (inc (:column caret 0))
        anchor-line (inc (or (:anchor-line caret) (:line caret 0)))
        anchor-column (inc (or (:anchor-column caret) (:column caret 0)))
        around (when node (common/definition-around node relative line))
        lines (some-> (get texts relative) common/text-lines)
        [first-line last-line] (sort [line anchor-line])
        selecting? (not= [line column] [anchor-line anchor-column])]
    (concat
     [(str "Their caret is " (if (= :cloud (:source caret)) "in the cloud" "in a VS Code editor") ", in "
           (or relative (str (:file caret) ", outside the cloud"))
           (when (and page-id (not= page-id relative)) (str " (" page-id ")"))
           ", at line " line ", column " column
           (when around (str ", inside " (:name around) " (" (common/line-span (:line around) (:end-line around)) ")"))
           (when (:at caret) (str ", " (ago now (:at caret))))
           ".")]
     (when lines
       (if selecting?
         (concat [(str "Selected from line " anchor-line ", column " anchor-column " to line " line ", column " column ":")]
                 (for [shown-line (range first-line (inc (min last-line (+ first-line (dec most-selected-lines)))))
                       :let [code (get lines (dec shown-line))]
                       :when code]
                   (str "  " shown-line "  " code))
                 (when (> (- last-line first-line) (dec most-selected-lines))
                   [(str "  and " (counted (- last-line first-line (dec most-selected-lines)) "more line" "more lines"))]))
         (when-let [code (not-empty (string/trim (get lines (dec line) "")))]
           [(str "Line " line ": " (common/clipped code longest-code))]))))))

(defn- viewer-focus [{:keys [snapshot focus open? visible? carets now] :as context} _arguments]
  (let [caret (:latest carets)
        most-listed 20
        nodes (:namespaces snapshot)
        editing (:editing focus)]
    {:text
     (string/join
      "\n"
      (concat
       [(cond
          (not open?) "No cloud is open in VS Code."
          (false? visible?) "The cloud is open, out of sight behind another tab."
          :else "The cloud is open and in sight.")]
       (if caret
         (caret-lines context caret)
         ["The person has not placed a caret in a file since VS Code started CodeCloud."])
       (when (and editing (not= :cloud (:source caret)))
         [(str "In the cloud, " (:path editing) " is open for editing.")])
       (when focus
         (concat
          (cond
            (:lifted focus) [(str "Lifted page: " (if-let [node (get nodes (:lifted focus))] (common/page-label node) (:lifted focus)) ".")]
            (:lifted-folder focus) [(str "Lifted folder: " (common/folder-label (:lifted-folder focus)) ".")]
            :else ["No page is lifted."])
          (when (and (:snapshot-id focus) (not= :working-tree (:snapshot-id focus)))
            [(str "The cloud shows " (or (:snapshot-label focus) (str "commit " (:snapshot-id focus))) " rather than the working tree.")])
          (if-let [in-view (seq (:in-view focus))]
            [(str "On screen, largest first" (when (:at focus) (str ", as of " (ago now (:at focus)))) ": "
                  (common/some-of in-view most-listed) ".")]
            ["No page is on screen."])))))}))

(def tool
  {:spec {"name" "viewer_focus"
          "title" "Where the person is looking"
          "description" (str "Where the person's caret and selection are, in the cloud or a VS Code editor, with the code "
                             "there, the page they lifted, and the pages on their screen.")
          "inputSchema" {"type" "object" "properties" {}}
          "annotations" common/read-only}
   :answer viewer-focus
   :paths-to-read (fn [{:keys [carets] :as context} _arguments]
                    (if-let [relative (some->> (:latest carets) (caret-path context))] [relative] []))
   :touched (fn [_context _arguments] [{:doing "checking where the person is looking"}])})
