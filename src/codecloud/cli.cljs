(ns codecloud.cli
  "Command-line entry point, which analyzes a folder without an editor and
  writes the data files the browser harness draws."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.config :as config]))

(def ^:private usage
  (string/join
   "\n"
   ["Usage: node dist/cli.js <command> <folder> [options]"
    ""
    "Commands:"
    "  summary   print the most important and most complected pages and every"
    "            cyclic dependency"
    "  bundle    write a data file for the browser harness"
    ""
    "Options:"
    "  --history <count>   commits to include in a bundle (default 0)"
    "  --out <file>        where bundle writes (default dev/data/<folder name>.edn)"
    "  --top <count>       namespaces summary lists (default 15)"]))

(defn- parse-arguments [arguments]
  (loop [remaining arguments
         options {:history 0 :top 15}]
    (if-let [argument (first remaining)]
      (case argument
        "--history" (recur (nnext remaining) (assoc options :history (js/parseInt (second remaining) 10)))
        "--out" (recur (nnext remaining) (assoc options :out (second remaining)))
        "--top" (recur (nnext remaining) (assoc options :top (js/parseInt (second remaining) 10)))
        (recur (next remaining)
               (cond
                 (nil? (:command options)) (assoc options :command argument)
                 (nil? (:folder options)) (assoc options :folder argument)
                 :else options)))
      options)))

(defn- padded [text width]
  (let [text (str text)]
    (if (>= (count text) width) text (str text (apply str (repeat (- width (count text)) " "))))))

(defn- counted [value singular plural]
  (str value " " (if (= 1 value) singular plural)))

(defn- print-summary [snapshot-value top-count notices problems]
  (let [{:keys [totals namespaces cycles]} snapshot-value
        ranked (take top-count (sort-by (comp - :importance) (vals namespaces)))
        name-width (reduce max 10 (map (comp count :name) ranked))]
    (doseq [line (concat problems notices)]
      (println "Note:" line))
    (println (str "Analyzed " (counted (:files totals) "file" "files") ": "
                  (counted (:namespaces totals) "namespace" "namespaces") ", "
                  (counted (:scripts totals) "script" "scripts") ", "
                  (counted (:modules totals 0) "module" "modules") ", "
                  (counted (:lines totals) "line" "lines") ", "
                  (counted (:cycles totals) "cyclic dependency" "cyclic dependencies") ", "
                  (counted (:cycle-warnings totals 0) "cyclic import" "cyclic imports") ", "
                  (counted (:parse-errors totals) "parse error" "parse errors") "."))
    (when (> (count (:languages totals)) 1)
      (println (str "Languages: "
                    (string/join ", " (map (fn [[language-id page-count]]
                                             (str (:label (catalog/language language-id)) " " page-count))
                                           (sort-by (comp - val) (:languages totals))))
                    ".")))
    (println)
    (println "Most important pages")
    (doseq [{namespace-name :name :keys [importance metrics]} ranked]
      (println (str "  " (.toFixed importance 2) "  " (padded namespace-name name-width)
                    "  functions " (padded (:functions metrics) 5)
                    "  references " (padded (:references metrics) 5)
                    "  lines " (:lines metrics))))
    (let [complected (->> (vals namespaces)
                          (filter #(pos? (get-in % [:metrics :complection] 0)))
                          (sort-by (comp - :complection :metrics))
                          (take top-count))]
      (when (seq complected)
        (println)
        (println "Most complected pages")
        (doseq [{namespace-name :name :keys [metrics hotspots]} complected]
          (println (str "  " (padded (.toFixed (:complection metrics) 2) 6) "  " (padded namespace-name name-width)
                        "  worst: " (string/join ", " (map :name (take 3 hotspots))))))))
    (doseq [[heading severity] [["Cyclic dependencies" :error] ["Cyclic imports" :warning]]
            :let [listed (filter #(= severity (:severity % :error)) cycles)]
            :when (seq listed)]
      (println)
      (println heading)
      (doseq [{:keys [path]} listed]
        (println (str "  " (string/join " -> " path)))))))

(defn- all-sources
  "Resolves to {namespace-id files} for every namespace of a snapshot."
  [project-analyzer configuration snapshot-value]
  (let [parallel-reads 8]
    (task/then (task/map-limited parallel-reads
                                 (fn [namespace-id]
                                   (task/then (analyzer/namespace-sources project-analyzer configuration
                                                                          snapshot-value namespace-id nil)
                                              (fn [namespace-files]
                                                [namespace-id (mapv #(dissoc % :content-key) namespace-files)])))
                                 (keys (:namespaces snapshot-value)))
               #(into {} %))))

(defn- commit-snapshots
  "Resolves to {sha snapshot} for `commits`, analyzed one at a time since
  each already reads its files in one git process."
  [project-analyzer configuration commits]
  (task/then (task/map-limited 1
                               (fn [commit]
                                 (task/then (analyzer/analyze-commit project-analyzer configuration commit)
                                            (fn [{:keys [snapshot]}] [(:sha commit) snapshot])))
                               commits)
             #(into {} %)))

(defn- bundle-of
  "Resolves to what the browser harness draws, the working tree's snapshot
  and sources with up to `history` commits and their snapshots."
  [project-analyzer configuration folder-name history working-snapshot]
  (task/then (task/all [(all-sources project-analyzer configuration working-snapshot)
                        (analyzer/history project-analyzer configuration history)])
             (fn [[sources commits]]
               (task/then (commit-snapshots project-analyzer configuration commits)
                          (fn [snapshots-by-sha]
                            {:root-name folder-name
                             :views (:views configuration)
                             :color-layers (config/color-layer-specs configuration)
                             :snapshot working-snapshot
                             :sources sources
                             :commits commits
                             :commit-snapshots snapshots-by-sha})))))

(defn- write-bundle! [project-analyzer configuration folder-name {:keys [history out]} working-snapshot]
  (let [out-path (or out (path/join "dev" "data" (str folder-name ".edn")))]
    (task/then (bundle-of project-analyzer configuration folder-name history working-snapshot)
               (fn [bundle]
                 (task/then (files/write-text out-path (pr-str bundle))
                            (fn [_] (println "Wrote" out-path "with" (count (:commits bundle)) "commits")))))))

(defn main [& arguments]
  (let [{:keys [command folder top] :as options} (parse-arguments arguments)]
    (if (or (not (contains? #{"summary" "bundle"} command)) (nil? folder))
      (do (println usage)
          (set! (.-exitCode js/process) 2))
      (let [root-directory (path/resolve folder)
            folder-name (path/basename root-directory)
            ;; The grammars sit beside this bundle in dist/.
            project-analyzer (analyzer/create-analyzer {:root-directory root-directory
                                                        :grammar-directory js/__dirname})
            cloud-file-path (files/absolute-path root-directory config/cloud-file-name)
            started-at (js/Date.now)]
        (-> (files/read-text cloud-file-path)
            (task/then #(analyzer/read-configuration project-analyzer %))
            (task/then
             (fn [{:keys [config problems]}]
               (task/then (analyzer/analyze-working-tree project-analyzer config {})
                          (fn [{:keys [snapshot notices]}]
                            (println (str "Analysis took " (- (js/Date.now) started-at) " ms"))
                            (if (= "summary" command)
                              (print-summary snapshot top notices problems)
                              (write-bundle! project-analyzer config folder-name options snapshot))))))
            (task/recover (fn [error]
                            (println "CodeCloud failed:" (or (.-message error) error))
                            (set! (.-exitCode js/process) 1))))))))

(set! *main-cli-fn* main)
