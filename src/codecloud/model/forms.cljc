(ns codecloud.model.forms
  "Questions about forms read from source, shared by the parsers.")

(defn head-name
  "Unqualified name of the symbol heading a list form, or nil."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (name (first form))))

(defn location
  "Line and column span the reader recorded on a form, or nil."
  [form]
  (let [{:keys [row col end-row end-col]} (meta form)]
    (when row
      {:line row :column col :end-line end-row :end-column end-col})))

(defn comment-form?
  "True for a (comment ...) block, whose code never runs and so is
  skipped when counting uses and measuring definitions."
  [form]
  (and (seq? form)
       (= "comment" (head-name form))
       (contains? #{nil "clojure.core" "cljs.core"} (namespace (first form)))))

(defn children
  "The forms directly inside `form`: a map's keys and values, or a
  sequence's items."
  [form]
  (if (map? form)
    (mapcat identity form)
    form))
