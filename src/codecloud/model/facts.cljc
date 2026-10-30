(ns codecloud.model.facts
  "The map every language adapter returns for one file, which is all the
  graph, the measurements, and the view read about that file.")

(def contract
  "Each key a file's facts carry. The Clojure adapter adds qualified
  :references and other tallies."
  {:path "The file's path from the cloud's root, with forward slashes"
   :language "The language id from codecloud.languages.catalog"
   :unit-id "The page the file belongs to, a namespace or the file's own path"
   :line-count "Lines as an editor counts them"
   :longest-line "Characters in the longest line"
   :requirements "What the file loads, each with a :target"
   :definitions "Each {:name :kind :line :end-line}"
   :top-level-forms "Spans of the file's top-level forms, statements, rules, or commands, each {:line :column :end-line :end-column}"
   :measured-forms "Definitions past a complection threshold, each with a :name and a :score"
   :flag-counts "Map of flag name to occurrences"
   :parse-error "{:message :line :column} when reading stopped early, or nil"})

(defn problems
  "Descriptions of the ways `facts` breaks the contract, empty when it
  keeps it."
  [facts]
  (let [counted? (fn [value] (and (int? value) (not (neg? value))))
        checks [[(string? (:path facts)) ":path must be a string."]
                [(keyword? (:language facts)) ":language must be a keyword."]
                [(string? (:unit-id facts)) ":unit-id must be a string."]
                [(counted? (:line-count facts)) ":line-count must be a count."]
                [(counted? (:longest-line facts)) ":longest-line must be a count."]
                [(every? :target (:requirements facts)) "Every requirement needs a :target."]
                [(every? #(and (string? (:name %)) (keyword? (:kind %))) (:definitions facts))
                 "Every definition needs a string :name and a keyword :kind."]
                [(every? :line (:top-level-forms facts)) "Every top-level form needs a :line."]
                [(every? #(and (:name %) (number? (:score %))) (:measured-forms facts))
                 "Every measured form needs a :name and a numeric :score."]
                [(map? (:flag-counts facts {})) ":flag-counts must be a map."]
                [(or (nil? (:parse-error facts)) (string? (get-in facts [:parse-error :message])))
                 ":parse-error must be nil or carry a :message."]]]
    (into [] (keep (fn [[kept? description]] (when-not kept? description))) checks)))
