(ns codecloud.agents.cloud-tools.page-details
  "The page_details tool, one page's definitions, dependencies, and
  tangles."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.counting :refer [counted]]
            [codecloud.presentation.page :as page]))

(def ^:private excess-words
  "How each of complection's measurements reads past its threshold."
  {:lines #(counted % "line")
   :branches #(counted % "branch" "branches")
   :nesting #(str "nesting " % " deep")
   :asynchronous #(counted % "asynchronous form")
   :state #(counted % "state change")})

(defn- excess-text [excesses]
  (string/join ", " (concat (keep (fn [[measurement value]]
                                    (when-let [words (get excess-words measurement)] (words value)))
                                  (sort-by key (dissoc excesses :flags)))
                            (when-let [flags (seq (:flags excesses))]
                              [(str "flagged " (string/join ", " (map (fn [[flag-name flag-count]] (str flag-name " " flag-count))
                                                                      (sort-by key flags))))]))))

(defn- definition-row [{definition-name :name :keys [kind private? line end-line inbound-count local-count]}]
  (str "  " definition-name " (" (when private? "private ") (string/replace (name (or kind :definition)) "-" " ")
       (when line (str ", " (common/line-span line end-line))) "): "
       (string/join ", " (remove nil? [(if (pos? (or inbound-count 0))
                                         (str (counted inbound-count "use") " from other pages")
                                         "no uses from other pages")
                                       (when (pos? (or local-count 0)) (str (counted local-count "use") " within"))]))))

(def ^:private most-neighbors
  "How many required or requiring pages, or outside requires, a page's
  details list before saying how many more there are."
  40)

(defn- summary-lines
  "The page's heading, docstring, measurements, cycle, and parse errors."
  [snapshot id node several-files?]
  (let [longest-doc 400
        files (:files node)
        {:keys [functions definitions references complection]} (:metrics node)
        found-cycle (some #(when (= (:cycle-id node) (:id %)) %) (:cycles snapshot))]
    (concat
     [(str id ": " (common/language-label (page/language-of node)) " " (common/kind-word node)
           (cond
             several-files? (str " in " (string/join " and " (map #(str (:path %) " (" (counted (:line-count % 0) "line") ")") files))
                                 ", " (counted (common/page-lines node) "line") " in all.")
             (page/named-by-path? node) (str ", " (counted (common/page-lines node) "line") ".")
             :else (str " in " (:path (first files)) ", " (counted (common/page-lines node) "line") ".")))]
     (when-let [doc (some-> (:doc node) (string/replace #"\s+" " ") string/trim not-empty)]
       [(str "Its docstring: " (common/clipped doc longest-doc))])
     [(str "Importance " (common/hundredths (:importance node 0)) ", " (counted (or functions 0) "function") ", "
           (counted (or definitions 0) "definition") ", " (counted (or references 0) "use") " from other pages"
           (when (pos? (or complection 0)) (str ", complection " (common/hundredths complection)))
           (when-let [flags (seq (:flag-counts node))]
             (str ", flagged " (string/join ", " (map (fn [[flag-name flag-count]] (str flag-name " " flag-count))
                                                     (sort-by key flags)))))
           ".")]
     (when found-cycle
       [(str (if (= :warning (:severity found-cycle)) "In a loop its languages allow: " "In a cycle that fails to load: ")
             (common/cycle-chain found-cycle))])
     (for [{:keys [path parse-error]} files
           :when parse-error]
       (str "Does not parse" (when several-files? (str " in " path)) ": " (:message parse-error)
            (when (:line parse-error) (str ", at line " (:line parse-error))) ".")))))

(defn- neighbor-lines [heading none neighbors note-of]
  (if (seq neighbors)
    (concat [(str heading " " (counted (count neighbors) "page") ":")]
            (map #(str "  " % " " (note-of %)) (take most-neighbors neighbors))
            (when (> (count neighbors) most-neighbors) [(str "  and " (- (count neighbors) most-neighbors) " more")]))
    [none]))

(defn- definition-lines [node several-files? limit]
  (let [ordered (sort-by (juxt :path #(or (:line %) 0)) (:definitions node))]
    (if (seq ordered)
      (concat [(str "Definitions (" (count ordered) "):")]
              (mapcat (fn [path-definitions]
                        (concat (when several-files? [(str " In " (:path (first path-definitions)) ":")])
                                (map definition-row path-definitions)))
                      (partition-by :path (take limit ordered)))
              (when (> (count ordered) limit)
                [(str "  and " (- (count ordered) limit) " more")]))
      ["No definitions."])))

(defn- hotspot-lines [node several-files?]
  (when-let [hotspots (seq (:hotspots node))]
    (concat ["" "Most tangled forms, worst first:"]
            (for [{hotspot-name :name :keys [path line end-line score excesses]} hotspots]
              (str "  " hotspot-name " (" (when several-files? (str path ", ")) (common/line-span line end-line) "): complection "
                   (common/hundredths score) (let [reasons (excess-text excesses)] (when (seq reasons) (str ", from " reasons))))))))

(defn- page-details [{:keys [snapshot] :as context} arguments]
  (let [{:keys [id error]} (common/find-page context (common/text-argument arguments "page"))]
    (if error
      {:error error}
      (let [node (get-in snapshot [:namespaces id])
            edges (common/edges-by-ends snapshot)
            several-files? (> (count (:files node)) 1)]
        {:text (string/join
                "\n"
                (concat (summary-lines snapshot id node several-files?)
                        [""]
                        (neighbor-lines "Requires" "Requires no page of the cloud."
                                        (remove #{id} (:requires node)) #(common/edge-note (get edges [id %])))
                        (neighbor-lines "Required by" "No page of the cloud requires it."
                                        (remove #{id} (:required-by node)) #(common/edge-note (get edges [% id])))
                        (when-let [external (seq (:external-requires node))]
                          [(str "Requires from outside the cloud: " (common/some-of (map str external) most-neighbors) ".")])
                        [""]
                        (definition-lines node several-files? (common/count-argument arguments "limit" 80 1 500))
                        (hotspot-lines node several-files?)))}))))

(def tool
  {:spec {"name" "page_details"
          "title" "One page"
          "description" (str "One page without reading its files: its definitions with their lines and uses, what it requires "
                             "and what requires it, and its most tangled forms.")
          "inputSchema" {"type" "object"
                         "properties" {"page" common/page-property
                                       "limit" {"type" "integer" "minimum" 1 "maximum" 500 "default" 80
                                                "description" "Most definitions to list"}}
                         "required" ["page"]}
          "annotations" common/read-only}
   :answer page-details
   :touched (fn [{:keys [snapshot] :as context} arguments]
              (let [{:keys [id]} (common/find-page context (common/text-argument arguments "page"))]
                (when id
                  [{:path (:path (page/primary-file (get-in snapshot [:namespaces id]))) :doing (str "studying " id)}])))})
