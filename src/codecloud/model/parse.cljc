(ns codecloud.model.parse
  "Facts about a Clojure file, read without evaluating it."
  (:require [clojure.string :as string]
            [codecloud.collections :as collections]
            [codecloud.model.complection :as complection]
            [codecloud.model.forms :as forms]
            [codecloud.model.ns-form :as ns-form]
            [codecloud.model.source :as source]
            [edamame.core :as edamame]))

(def function-kinds
  "Definition kinds that count toward a namespace's function total."
  #{:function :macro :multimethod :protocol-method :definition})

(defn- parse-options
  "Edamame options that read source for `features` without evaluating it."
  [features]
  (edamame/normalize-opts
   {:all true
    :read-cond :allow
    :features features
    :auto-resolve-ns true
    ;; An alias the file never declared resolves to itself.
    :auto-resolve (fn [alias] (if (= alias :current) 'user alias))
    ;; Tagged literals read as their bare value.
    :readers (fn [_tag] identity)
    ;; Regexes stay data, since a Java pattern can be invalid in JavaScript.
    :regex (fn [pattern] (list 're-pattern pattern))
    ;; A syntax-quoted template keeps the shape it is written in, since its
    ;; expansion nests far deeper than the source does.
    :syntax-quote identity
    :location? seq?}))

(defn- error-description [error]
  (let [data (ex-data error)]
    {:message (or (ex-message error) (str error))
     :line (:row data)
     :column (:col data)}))

(defn read-forms
  "{:forms :error} for normalized text read with `features`. Reading stops
  at the first syntax error, keeping the forms before it."
  [text features]
  (let [options (parse-options features)
        reader (edamame/reader text)]
    (loop [forms (transient [])]
      (let [outcome (try
                      {:form (edamame/parse-next reader options)}
                      (catch #?(:clj Exception :cljs :default) error
                        {:error (error-description error)}))]
        (cond
          (:error outcome) {:forms (persistent! forms) :error (:error outcome)}
          (= :edamame.core/eof (:form outcome)) {:forms (persistent! forms) :error nil}
          :else (recur (conj! forms (:form outcome))))))))

(defn- function-value?
  "True when the value of a `def` is evidently a function."
  [value-form]
  (contains? #{"fn" "fn*" "partial" "comp" "complement" "juxt" "memoize"
               "constantly" "some-fn" "every-pred" "fnil"}
             (forms/head-name value-form)))

(defn- definition-kind [head-text value-form]
  (case head-text
    ("defn" "defn-") :function
    "defmacro" :macro
    "defmulti" :multimethod
    ("def" "defonce") (if (function-value? value-form) :function :value)
    "defprotocol" :protocol
    ("defrecord" "deftype" "definterface" "defstruct") :type
    ("deftest" "defspec") :test
    :definition))

(defn- defining-form?
  "True for a form whose head starts with def and whose second item names
  a var, which also covers library macros such as defstate."
  [form]
  (let [head-text (forms/head-name form)]
    (and head-text
         (string/starts-with? head-text "def")
         (not (contains? #{"defmethod" "default" "defaults"} head-text))
         (symbol? (second form)))))

(defn- protocol-method-definitions [protocol-form]
  (into []
        (comp (filter #(and (seq? %) (symbol? (first %))))
              (map (fn [signature]
                     (let [{:keys [line end-line]} (forms/location signature)]
                       {:name (name (first signature))
                        :kind :protocol-method
                        :private? false
                        :line line
                        :end-line end-line}))))
        (drop 2 protocol-form)))

(defn- definitions-of-form [form]
  (when (defining-form? form)
    (let [head-text (forms/head-name form)
          name-symbol (second form)
          kind (definition-kind head-text (last form))
          {:keys [line end-line]} (forms/location form)
          definition {:name (name name-symbol)
                      :kind kind
                      :private? (or (= "defn-" head-text)
                                    (boolean (:private (meta name-symbol))))
                      :line line
                      :end-line end-line}]
      (if (= :protocol kind)
        (into [definition] (protocol-method-definitions form))
        [definition]))))

(defn- reduce-symbols
  "Reduces `step` over every symbol inside `form`, skipping comment blocks."
  [step initial form]
  (cond
    (symbol? form) (step initial form)
    (forms/comment-form? form) initial
    (coll? form) (reduce (fn [accumulated item] (reduce-symbols step accumulated item))
                         initial
                         form)
    :else initial))

(defn- classify-symbol
  "[:qualified \"ns/name\"], [:local \"name\"], [:unresolved \"name\"], or
  nil for locals, special forms, and core."
  [{:keys [namespace-name aliases refers own-names track-unresolved?]} symbol-value]
  (let [qualifier (namespace symbol-value)
        simple-name (name symbol-value)]
    (cond
      qualifier
      (let [target (get aliases qualifier qualifier)]
        (if (= target namespace-name)
          [:local simple-name]
          [:qualified (str target "/" simple-name)]))

      (contains? refers simple-name) [:qualified (get refers simple-name)]
      (contains? own-names simple-name) [:local simple-name]
      track-unresolved? [:unresolved simple-name]
      :else nil)))

(defn- count-reference [tallies symbol-value context]
  (if-let [[category reference] (classify-symbol context symbol-value)]
    (update-in tallies [category reference] (fnil inc 0))
    tallies))

(defn- reference-scope
  "The part of a top-level form whose symbols count as uses, leaving out a
  definition's head and name."
  [form]
  (if (defining-form? form) (drop 2 form) form))

(defn- requirement-form? [form]
  (or (some? (ns-form/namespace-declaration form))
      (some? (ns-form/require-call-requirements form))))

(defn- top-level-form-summary [form]
  (when-let [span (forms/location form)]
    (assoc span
           :head (forms/head-name form)
           :name (when (symbol? (second form)) (name (second form))))))

(defn- platform-of
  "The platform a feature set reads code for. Requirements keep it, since
  a .cljc file can require different namespaces on each platform."
  [features]
  (cond
    (contains? features :cljs) :cljs
    (contains? features :cljd) :cljd
    (contains? features :cljr) :cljr
    :else :clj))

(defn- measured-name
  "The inspector's name for a measured top-level form, or nil for data,
  declarations, and tests."
  [form]
  (let [head-text (forms/head-name form)
        unmeasured-kinds #{:value :protocol :test}]
    (cond
      (= "defmethod" head-text) (string/join " " (map pr-str (take 2 (rest form))))
      (contains? #{"extend-protocol" "extend-type"} head-text) (str head-text " " (pr-str (second form)))
      (and (defining-form? form)
           (not (contains? unmeasured-kinds (definition-kind head-text (last form)))))
      (name (second form))
      :else nil)))

(defn- measured-form
  "How tangled one top-level form is, as {:name :line :end-line :score
  :excesses}, or nil when it holds no code or scores zero."
  [form context {:keys [compiled-flags thresholds]} text-lines]
  (when-let [form-name (measured-name form)]
    (let [measurements (complection/measure form context compiled-flags text-lines)
          form-score (complection/score measurements thresholds)]
      (when (pos? form-score)
        (let [{:keys [line end-line]} (forms/location form)]
          {:name form-name
           :line line
           :end-line end-line
           :score (complection/rounded form-score)
           :excesses (complection/excesses measurements thresholds)})))))

(defn- symbol-flag-counts
  "Map of flag name to how many flagged symbols `forms` use, read through
  the file's aliases and refers, skipping declarations and requires."
  [compiled-flags context forms]
  (reduce (fn [found form]
            (if (requirement-form? form)
              found
              (reduce-symbols (fn [inner symbol-value]
                                (reduce (fn [counts flag-name] (update counts flag-name (fnil inc 0)))
                                        inner
                                        (complection/symbol-flags compiled-flags context symbol-value)))
                              found
                              form)))
          {}
          forms))

(defn- file-context
  "What resolving a symbol needs: the namespace, aliases, refers, own
  definitions, and whether :refer :all leaves names to guess."
  [declaration requirements definitions]
  {:namespace-name (:name declaration)
   :aliases (ns-form/alias-table requirements)
   :refers (ns-form/refer-table requirements)
   :own-names (into #{} (map :name) definitions)
   :track-unresolved? (boolean (some #(and (:refer-all? %) (not (:js-module? %))) requirements))})

(defn- reference-tallies
  "Uses in `forms` by category and name: {:qualified {\"ns/name\" count}
  :local {...} :unresolved {...}}."
  [forms context]
  (reduce (fn [tallies form]
            (if (requirement-form? form)
              tallies
              (reduce-symbols (fn [inner symbol-value] (count-reference inner symbol-value context))
                              tallies
                              (reference-scope form))))
          {}
          forms))

(def ^:private command-calls
  "Names of the functions that run a command line, from babashka.process,
  babashka.tasks, and clojure.java.shell."
  #{"shell" "sh" "process" "exec"})

(defn- command-text
  "The command line a call runs, from its string arguments or a vector of
  strings, or nil when no argument is a literal string."
  [call]
  (let [words (mapcat (fn [argument]
                        (cond
                          (string? argument) [argument]
                          (and (vector? argument) (every? string? argument)) argument
                          :else []))
                      (rest call))]
    (when (seq words) (string/join " " words))))

(defn- commands-in
  "{:text :line} for each command line `form` runs through a shell call
  written with literal strings."
  [form]
  (cond
    (forms/comment-form? form) []
    (and (seq? form) (contains? command-calls (forms/head-name form)))
    (into (if-let [text (command-text form)]
            [{:text text :line (:line (forms/location form))}]
            [])
          (mapcat commands-in (rest form)))
    (coll? form) (into [] (mapcat commands-in) (forms/children form))
    :else []))

(defn- facts-for-pass
  "Facts from reading the text once with one feature set."
  [text text-lines features {:keys [compiled-flags] :as measuring}]
  (let [{:keys [forms error]} (read-forms text features)
        declaration (some ns-form/namespace-declaration forms)
        platform (platform-of features)
        requirements (into []
                           (map #(assoc % :platform platform))
                           (concat (:requirements declaration)
                                   (mapcat ns-form/require-call-requirements forms)))
        definitions (into [] (mapcat definitions-of-form) forms)
        context (file-context declaration requirements definitions)
        tallies (reference-tallies forms context)]
    {:namespace (:name declaration)
     :doc (:doc declaration)
     :requirements requirements
     :definitions definitions
     :method-count (count (filter #(= "defmethod" (forms/head-name %)) forms))
     :references (get tallies :qualified {})
     :local-references (get tallies :local {})
     :unresolved (get tallies :unresolved {})
     :top-level-forms (into [] (keep top-level-form-summary) forms)
     :class-imports (vec (:class-imports declaration))
     :commands (into [] (mapcat commands-in) forms)
     :measured-forms (into [] (keep #(measured-form % context measuring text-lines)) forms)
     :flag-counts (merge-with + (complection/text-flags compiled-flags text)
                              (symbol-flag-counts compiled-flags context forms))
     :parse-error error}))

(defn- merge-passes
  "One file's facts from the facts of each reading pass."
  [path extension text-lines passes]
  {:path path
   :dialect extension
   :line-count (count text-lines)
   :longest-line (reduce max 0 (map count text-lines))
   :namespace (some :namespace passes)
   :doc (some :doc passes)
   :requirements (vec (distinct (mapcat :requirements passes)))
   :class-imports (vec (distinct (mapcat :class-imports passes)))
   :commands (vec (distinct (mapcat :commands passes)))
   :definitions (collections/distinct-by :name (mapcat :definitions passes))
   :method-count (reduce max 0 (map :method-count passes))
   :references (apply merge-with max (map :references passes))
   :local-references (apply merge-with max (map :local-references passes))
   :unresolved (apply merge-with max (map :unresolved passes))
   :top-level-forms (->> (mapcat :top-level-forms passes)
                         (collections/distinct-by (juxt :line :column))
                         (sort-by (juxt :line :column))
                         vec)
   :measured-forms (->> (mapcat :measured-forms passes)
                        (group-by :line)
                        vals
                        (map #(apply max-key :score %))
                        (sort-by :line)
                        vec)
   :flag-counts (apply merge-with max (map :flag-counts passes))
   :parse-error (some :parse-error passes)})

(defn analyze-file
  "Facts about the Clojure file at `path`, with the cloud file's `flags`
  and `complection` thresholds."
  ([path text] (analyze-file path text {}))
  ([path text {:keys [flags complection]}]
   (let [normalized-text (source/normalize-text text)
         text-lines (source/split-lines normalized-text)
         extension (source/file-extension path)
         ;; A file shared between platforms is read once for each, so
         ;; requires and definitions behind every #? branch are seen.
         features-by-extension {"clj" [#{:clj}]
                                "cljs" [#{:cljs}]
                                "cljc" [#{:clj} #{:cljs}]
                                "cljd" [#{:cljd}]
                                "cljr" [#{:cljr}]
                                "bb" [#{:bb :clj}]}
         feature-sets (get features-by-extension extension [#{:clj}])
         measuring {:compiled-flags (complection/compile-flags flags)
                    :thresholds (complection/thresholds-with complection)}]
     (merge-passes path extension text-lines
                   (mapv #(facts-for-pass normalized-text text-lines % measuring) feature-sets)))))
