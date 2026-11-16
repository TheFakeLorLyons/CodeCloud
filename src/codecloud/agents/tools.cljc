(ns codecloud.agents.tools
  "What the hub tells agents about itself, and the seven tools it lists
  from wavescope-mcp, described in CodeCloud's words and taking paths
  relative to the workspace as well as absolute ones. The hub resolves
  each path before passing a call on to wavescope, whose own argument
  names and limits the schemas keep, and reshapes two of wavescope's
  answers on the way back: the code around a line keeps only its fine
  band unless every band is wanted, and a summary's lines come in order.
  The cloud's own tools are codecloud.agents.cloud-tools'."
  (:require [clojure.string :as string]
            [codecloud.agents.presence :as presence]))

(def reporting-levels
  "How much agents report of their work to the person, loudest first.
  Each report costs the agent tokens, so quieter levels cost less."
  [:full :agents :brief :off])

(defn- reporting-guidance [reporting]
  (let [pins-and-coworkers (str "point_out pins a note to a line for the person to click, and coworkers lists the "
                                "other agents at work, so you can keep clear of a file another agent is changing.")]
    (case reporting
      :full (str "Call announce_work on starting with a file, saying in a few words what the work is, so the person "
                 "can see it even while you read and write with other tools; a subagent passes its own name. "
                 pins-and-coworkers)
      :agents (str "Call announce_work on starting with a file, saying in a few words what the work is, so the person "
                   "can see it even while you read and write with other tools. A subagent leaves announce_work to the "
                   "agent that started it. " pins-and-coworkers)
      :brief (str "Call announce_work once per task, as the task starts, naming the file it starts in and the task in "
                  "a few words. A subagent leaves it to the agent that started it.")
      nil)))

(defn instructions
  "What the hub tells every agent about itself when it connects, saying
  as much about reporting as `reporting` asks."
  [reporting]
  (string/join
   " "
   (remove nil?
           ["CodeCloud shows the person this codebase as a cloud of pages, one for each namespace or file, and shows them where each agent works."
            (reporting-guidance reporting)
            (str "Start from the cloud's tools: cloud_overview gives the codebase's shape, page_details one page's "
                 "definitions and dependencies without reading it, uses_of every line using a definition, and "
                 "path_between why two pages are tied. Prefer page_details and uses_of to reading whole files. "
                 "viewer_focus and cloud_view say what the person is looking at, so work can start there. wavescope's "
                 "tools read files by their structure at several resolutions: outline a long file with "
                 "get_summary_at_scale or query_wavelet_context and read only the lines that matter; "
                 "get_cursor_context reads around the person's caret. Paths may be relative to the workspace folder. "
                 "The cloud's tools count lines from 1, as editors do, and wavescope's count from 0.")])))

(def path-arguments
  "Arguments that name files or folders, which the hub makes absolute and
  keeps within the workspace before wavescope sees them."
  #{"file" "directory"})

(def cursor-tools
  "wavescope's tools that read around the person's caret. The hub tells
  wavescope where the caret is before passing a call on."
  #{"get_cursor_context" "get_cursor_important_positions"})

(def ^:private file-property
  {"type" "string"
   "description" "Path to the file, absolute or relative to the workspace folder"})

(def ^:private read-only
  "Annotations telling clients a tool only reads, so they may run it
  without asking first."
  {"readOnlyHint" true "openWorldHint" false})

(def wavescope-tools
  "wavescope-mcp 1.0.14's tools that agents get."
  [{"name" "query_wavelet_context"
    "title" "Code around a line"
    "description" (str "A file around one line at three resolutions: the exact lines nearest the center, "
                       "the signatures of functions and classes further out, and section-level structure "
                       "beyond. Zooms in and out of a file without reading all of it.")
    "inputSchema" {"type" "object"
                   "properties" {"file" file-property
                                 "center" {"type" "integer" "minimum" 0
                                           "description" "Line to center on, counting from 0"}
                                 "radius" {"type" "integer" "minimum" 10 "maximum" 2000 "default" 300
                                           "description" "Lines to consider before and after the center together"}}
                   "required" ["file" "center"]}
    "annotations" read-only}
   {"name" "get_summary_at_scale"
    "title" "Summary of a range"
    "description" (str "A compressed view of a range of lines built from the structural peaks at one scale; "
                       "larger scales summarize more coarsely. Leaving out the scale fits it to the range.")
    "inputSchema" {"type" "object"
                   "properties" {"file" file-property
                                 "start" {"type" "integer" "minimum" 0
                                          "description" "First line, counting from 0"}
                                 "end" {"type" "integer" "minimum" 0
                                        "description" "Last line, counting from 0, included"}
                                 "scale" {"type" "integer" "minimum" 1 "maximum" 128
                                          "description" "Wavelet scale: 1 and 2 fine, 4 to 16 medium, 32 to 128 coarse"}}
                   "required" ["file" "start" "end"]}
    "annotations" read-only}
   {"name" "get_important_positions"
    "title" "Landmarks"
    "description" (str "Structurally important lines, such as function and class boundaries and imports, "
                       "in one file or across a project, strongest first. Given neither a file nor a "
                       "directory, it searches the workspace folder.")
    "inputSchema" {"type" "object"
                   "properties" {"file" {"type" "string"
                                         "description" "One file, absolute or relative to the workspace folder"}
                                 "directory" {"type" "string"
                                              "description" "A project directory, absolute or relative to the workspace folder"}
                                 "min_coefficient" {"type" "number" "minimum" 0 "maximum" 20 "default" 0.3
                                                    "description" "Weakest landmark to include; lower finds more"}
                                 "limit" {"type" "integer" "minimum" 1 "maximum" 100 "default" 20
                                          "description" "Most landmarks to return"}}}
    "annotations" read-only}
   {"name" "diff_wavelet_context"
    "title" "Structural changes"
    "description" (str "How a file's structure changed between two git revisions: boundaries added, removed, "
                       "shifted, or changed in strength. Leaving out targetRef compares with the working tree.")
    "inputSchema" {"type" "object"
                   "properties" {"file" file-property
                                 "baseRef" {"type" "string"
                                            "description" "Base revision, such as HEAD~1, main, or a commit hash"}
                                 "targetRef" {"type" "string"
                                              "description" "Target revision; leave it out for the working tree"}
                                 "minCoefficient" {"type" "number" "minimum" 0 "maximum" 20 "default" 0.3
                                                   "description" "Weakest boundary to compare; lower finds more"}
                                 "limit" {"type" "integer" "minimum" 1 "maximum" 500 "default" 100
                                          "description" "Most boundaries to find in each revision"}
                                 "window" {"type" "integer" "minimum" 1 "maximum" 50 "default" 2
                                           "description" "Farthest a boundary may move, in lines, and still count as shifted"}}
                   "required" ["file" "baseRef"]}
    "annotations" read-only}
   {"name" "get_complexity_heatmap"
    "title" "Dense code"
    "description" (str "Per-line irregularity scores for a file, higher where code is structurally dense, "
                       "for deciding what to review first and what to summarize.")
    "inputSchema" {"type" "object"
                   "properties" {"file" file-property
                                 "lossy_bits" {"type" "integer" "minimum" 0 "maximum" 8 "default" 0
                                               "description" "Low bits dropped before scoring; 0 keeps full precision"}}
                   "required" ["file"]}
    "annotations" read-only}
   {"name" "get_cursor_context"
    "title" "Code around the person's caret"
    "description" (str "wavescope's three resolutions around the person's caret, in the file it is in, or in "
                       "another file it was last in when that file is named.")
    "inputSchema" {"type" "object"
                   "properties" {"file" {"type" "string"
                                         "description" (str "A file the caret has been in, absolute or relative to the "
                                                            "workspace folder; leave it out for the caret's own file")}}}
    "annotations" read-only}
   {"name" "get_cursor_important_positions"
    "title" "Landmarks near the person's caret"
    "description" "Structurally important lines near the person's caret, ranked by nearness and strength."
    "inputSchema" {"type" "object"
                   "properties" {"file" {"type" "string"
                                         "description" (str "A file the caret has been in, absolute or relative to the "
                                                            "workspace folder; leave it out for the caret's own file")}
                                 "limit" {"type" "integer" "minimum" 1 "maximum" 50 "default" 10
                                          "description" "Most landmarks to return"}}}
    "annotations" read-only}])

(def wavescope-bands
  "How much of wavescope's view around a line agents get, smallest
  first: :fine, the lines nearest it in full, or :all, adding the
  signatures and sections further out and the strongest peaks nearby,
  which help most in a large codebase."
  [:fine :all])

(def band-tools
  "wavescope's tools answering with bands at several resolutions around
  a line."
  #{"query_wavelet_context" "get_cursor_context"})

(def ^:private fine-band-descriptions
  "What the tools reading around a line say of themselves while they
  return the fine band alone."
  {"query_wavelet_context" (str "The exact lines of a file nearest one line, a fifth of the radius on either side and "
                                "at least ten. Reads part of a file without reading all of it; get_summary_at_scale "
                                "outlines a longer range.")
   "get_cursor_context" (str "The exact lines nearest the person's caret, in the file it is in, or in another file it "
                             "was last in when that file is named.")})

(defn listed-wavescope-tools
  "wavescope's tools as agents see them, those reading around a line
  described for `bands`, one of wavescope-bands."
  [bands]
  (if (= :all bands)
    wavescope-tools
    (mapv (fn [tool]
            (if-let [description (get fine-band-descriptions (get tool "name"))]
              (assoc tool "description" description)
              tool))
          wavescope-tools)))

(defn fine-band-only
  "wavescope's answer about the code around a line, parsed with string
  keys, keeping of its bands the fine one alone and leaving out its
  peaks, which mostly repeat the fine band's lines."
  [answer]
  (if (map? (get answer "bands"))
    (-> answer
        (update "bands" select-keys ["fine"])
        (dissoc "waveletPeaks"))
    answer))

(defn summary-by-line
  "get_summary_at_scale's text with its lines in the order of the lines
  they name. wavescope lists a summary's peaks strongest first and marks
  the gaps between them in that order, so the peaks are sorted and the
  gaps between them marked again over the same range. Text holding no
  peaks, such as the summary of a range without any, comes back as it
  is."
  [text]
  (let [read-entry (fn [line]
                     (if-let [[_ position content] (re-matches #"\[(\d+)\] (.*)" line)]
                       {:peak (parse-long position) :content content}
                       (when-let [[_ first-line last-line] (re-matches #"\[(\d+)-(\d+)\] \.\.\." line)]
                         {:gap [(parse-long first-line) (parse-long last-line)]})))
        entries (map read-entry (string/split-lines text))
        peaks (->> entries (filter :peak) (sort-by :peak) (partition-by :peak) (map first))]
    (if (or (empty? peaks) (some nil? entries))
      text
      (let [named-lines (mapcat #(if (:peak %) [(:peak %)] (:gap %)) entries)
            range-end (apply max named-lines)
            gap (fn [first-line last-line] (str "[" first-line "-" last-line "] ..."))
            [parts last-shown] (reduce (fn [[parts previous] {:keys [peak content]}]
                                         [(cond-> parts
                                            (> peak (inc previous)) (conj (gap (inc previous) (dec peak)))
                                            true (conj (str "[" peak "] " content)))
                                          peak])
                                       [[] (dec (apply min named-lines))]
                                       peaks)]
        (string/join "\n" (cond-> parts
                             (< last-shown range-end) (conj (gap (inc last-shown) range-end))))))))

(defn wavescope-read
  "What a call of wavescope's `tool-name` read, as {:path :start :end
  :line :doing} with one-based lines; `shown-range` holds the zero-based
  lines shown in full."
  [tool-name arguments shown-range]
  (let [file (get arguments "file")
        file-name (presence/file-name file)
        center (let [given (get arguments "center")] (when (number? given) (max 0 (int given))))
        ;; wavescope shows the lines within a fifth of its radius of the
        ;; center in full, and at least ten either side; its radius is 300
        ;; unless a call gives another.
        default-radius 300
        least-reach 10
        shown (cond
                (and (sequential? shown-range) (= 2 (count shown-range)) (every? number? shown-range))
                {:start (inc (int (first shown-range))) :end (inc (int (second shown-range)))}

                center
                (let [radius (let [given (get arguments "radius")] (if (number? given) (int given) default-radius))
                      reach (max least-reach (quot radius 5))]
                  {:start (inc (max 0 (- center reach))) :end (inc (+ center reach))}))
        at-center (when center {:line (inc center)})]
    (case tool-name
      "query_wavelet_context" (merge {:path file :doing (str "reading " file-name " around line " (inc (or center 0)))}
                                     shown at-center)
      "get_cursor_context" (merge {:path file :doing "reading around the person's caret"} shown at-center)
      "get_cursor_important_positions" (merge {:path file :doing "finding landmarks near the person's caret"} at-center)
      "get_summary_at_scale" (let [start (get arguments "start")
                                   end (get arguments "end")]
                               (if (and (number? start) (number? end))
                                 (let [first-line (inc (max 0 (int start)))
                                       last-line (max first-line (inc (int end)))]
                                   {:path file :start first-line :end last-line
                                    :doing (str "summarizing " file-name ", lines " first-line "-" last-line)})
                                 {:path file :doing (str "summarizing " file-name)}))
      "get_important_positions" (if file
                                  {:path file :doing (str "finding landmarks in " file-name)}
                                  {:doing (str "finding landmarks across " (presence/file-name (get arguments "directory")))})
      "diff_wavelet_context" {:path file :doing (str "comparing " file-name " with " (get arguments "baseRef"))}
      "get_complexity_heatmap" {:path file :doing (str "finding the dense code in " file-name)}
      nil)))
