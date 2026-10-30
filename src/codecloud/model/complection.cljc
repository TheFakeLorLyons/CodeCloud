(ns codecloud.model.complection
  "How tangled each top-level definition is, from its length, branches,
  nesting, asynchronous calls, state changes, and flags. Points count
  only past a plain function, so straightforward code scores zero."
  (:require [clojure.string :as string]
            [codecloud.model.forms :as forms]))

(def asynchronous-calls
  "Calls that start work for later or wait on it, by qualified name, or by
  bare name for clojure.core functions and JavaScript methods."
  #{"clojure.core.async/go" "clojure.core.async/go-loop" "clojure.core.async/thread"
    "clojure.core.async/thread-call" "clojure.core.async/<!" "clojure.core.async/>!"
    "clojure.core.async/<!!" "clojure.core.async/>!!" "clojure.core.async/alts!"
    "clojure.core.async/alts!!" "clojure.core.async/put!" "clojure.core.async/take!"
    "clojure.core.async/timeout" "clojure.core.async/pipeline"
    "clojure.core.async/pipeline-async" "clojure.core.async/pipeline-blocking"
    "cljs.core.async/go" "cljs.core.async/go-loop" "cljs.core.async/<!" "cljs.core.async/>!"
    "cljs.core.async/alts!" "cljs.core.async/put!" "cljs.core.async/take!" "cljs.core.async/timeout"
    "cljs.core.async.macros/go" "cljs.core.async.macros/go-loop"
    "future" "future-call" "promise" "deliver" "pmap" "pcalls" "pvalues"
    "send" "send-off" "send-via"
    "js/setTimeout" "js/setInterval" "js/requestAnimationFrame" "js/queueMicrotask" "js/fetch"
    "js/Promise." "js/Promise.all" "js/Promise.race" ".then" ".catch" ".finally"
    "promesa.core/let" "promesa.core/then" "promesa.core/chain" "promesa.core/all"
    "promesa.core/future" "promesa.core/create" "promesa.core/delay"
    "manifold.deferred/chain" "manifold.deferred/let-flow" "manifold.deferred/future"
    "manifold.deferred/catch" "manifold.deferred/zip"})

(def state-changes
  "Calls that change mutable state in place."
  #{"swap!" "reset!" "swap-vals!" "reset-vals!" "vswap!" "vreset!" "set!" "aset"
    "alter" "ref-set" "commute" "alter-var-root"})

(def default-thresholds
  "For each measurement, how much a plain definition may have, and how
  much more past that adds a point."
  {:lines {:allowed 25 :per-point 25}
   :branches {:allowed 6 :per-point 6}
   :nesting {:allowed 7 :per-point 4}
   :asynchronous {:allowed 1 :per-point 1}
   :state {:allowed 2 :per-point 3}})

(defn thresholds-with
  "The default thresholds with allowed amounts taken from `allowed`, a map
  such as {:lines 40 :branches 8}."
  [allowed]
  (reduce-kv (fn [thresholds measurement amount]
               (assoc-in thresholds [measurement :allowed] amount))
             default-thresholds
             (select-keys allowed (keys default-thresholds))))

(defn- core-name
  "A name without a clojure.core or cljs.core qualifier."
  [qualified-name]
  (string/replace qualified-name #"^(?:clojure|cljs)\.core/" ""))

(defn- qualified-name
  "A symbol's name, qualified through the file's aliases and refers where
  they cover it."
  [{:keys [aliases refers]} symbol-value]
  (let [qualifier (namespace symbol-value)
        simple-name (name symbol-value)]
    (cond
      qualifier (str (get aliases qualifier qualifier) "/" simple-name)
      (contains? refers simple-name) (get refers simple-name)
      :else simple-name)))

(defn call-name
  "Qualified name of what a list form calls, or nil for anything but a
  call."
  [context form]
  (when (and (seq? form) (symbol? (first form)))
    (core-name (qualified-name context (first form)))))

(defn- branches-of
  "How many code paths a form adds, which is one for a simple conditional
  or catch and one for each clause of a multi-way branch."
  [form]
  (let [clause-pairs (fn [leading-arguments]
                       (quot (max 0 (- (count form) 1 leading-arguments)) 2))]
    (case (forms/head-name form)
      ("if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-some" "when-first" "catch") 1
      "cond" (clause-pairs 0)
      ("cond->" "cond->>" "case") (clause-pairs 1)
      "condp" (clause-pairs 2)
      0)))

(defn compile-flags
  "Indexes flags {name {:text :symbols}} as {:text [[name pattern]]
  :by-symbol {pattern [name]} :by-name {pattern [name]}}."
  [flags]
  (reduce-kv (fn [index flag-name {:keys [text symbols]}]
               (reduce (fn [inner pattern]
                         (if (string/includes? pattern "/")
                           (update-in inner [:by-symbol pattern] (fnil conj []) flag-name)
                           (update-in inner [:by-name pattern] (fnil conj []) flag-name)))
                       (update index :text into (map (fn [pattern] [flag-name pattern]) text))
                       symbols))
             {:text [] :by-symbol {} :by-name {}}
             (or flags {})))

(defn symbol-flags
  "Names of the flags a symbol raises, matching it as written, as the
  file's aliases and refers qualify it, and by its bare name."
  [{:keys [by-symbol by-name]} context symbol-value]
  (let [written (str symbol-value)
        qualified (qualified-name context symbol-value)]
    (distinct (concat (get by-symbol written)
                      (when (not= qualified written) (get by-symbol qualified))
                      (get by-name (name symbol-value))))))

(defn- occurrences
  "How many times `pattern` appears in `text` without overlapping."
  [text pattern]
  (loop [from 0
         found 0]
    (if-let [index (when (seq pattern) (string/index-of text pattern from))]
      (recur (+ index (count pattern)) (inc found))
      found)))

(defn text-flags
  "Map of flag name to occurrences of its text patterns in `text`."
  [compiled-flags text]
  (reduce (fn [found [flag-name pattern]]
            (let [hits (occurrences text pattern)]
              (if (pos? hits) (update found flag-name (fnil + 0) hits) found)))
          {}
          (:text compiled-flags)))

(defn- tally
  "`tallies` with what one form inside a definition adds."
  [context compiled-flags tallies form depth]
  (let [called (call-name context form)]
    (cond-> (update tallies :branches + (branches-of form))
      (seq? form) (update :nesting max depth)
      (contains? asynchronous-calls called) (update :asynchronous inc)
      (contains? state-changes called) (update :state inc)
      (symbol? form) (update :flags #(reduce (fn [found flag-name] (update found flag-name (fnil inc 0)))
                                             %
                                             (symbol-flags compiled-flags context form))))))

(defn- walk
  "Folds `step` over `form` and its descendants, skipping comment blocks,
  passing the number of enclosing lists."
  [step initial form]
  (letfn [(visit [accumulated item depth]
            (cond
              (forms/comment-form? item) accumulated
              (coll? item) (let [inner-depth (if (seq? item) (inc depth) depth)]
                             (reduce (fn [inner child] (visit inner child inner-depth))
                                     (step accumulated item depth)
                                     (forms/children item)))
              :else (step accumulated item depth)))]
    (visit initial form 0)))

(defn span-text
  "Text of the one-based lines `first-line` through `last-line`."
  [text-lines first-line last-line]
  (let [start (max 0 (dec (or first-line 1)))
        end (min (count text-lines) (or last-line first-line 1))]
    (if (< start end)
      (string/join "\n" (subvec text-lines start end))
      "")))

(defn measure
  "Measurements of a top-level form as {:lines :branches :nesting
  :asynchronous :state :flags}, :flags mapping names to counts."
  [form context compiled-flags text-lines]
  (let [{:keys [line end-line]} (forms/location form)
        symbol-tallies (walk (fn [tallies item depth] (tally context compiled-flags tallies item depth))
                             {:branches 0 :nesting 0 :asynchronous 0 :state 0 :flags {}}
                             form)]
    (assoc symbol-tallies
           :lines (if (and line end-line) (inc (- end-line line)) 1)
           :flags (if (seq (:text compiled-flags))
                    (merge-with + (:flags symbol-tallies)
                                (text-flags compiled-flags (span-text text-lines line end-line)))
                    (:flags symbol-tallies)))))

(defn score
  "Points for a definition's measurements, one for each step past a
  threshold and half of one for each flagged occurrence."
  [measurements thresholds]
  (let [points-per-flag 0.5]
    (+ (reduce-kv (fn [total measurement {:keys [allowed per-point]}]
                    (+ total (/ (max 0 (- (get measurements measurement 0) allowed)) per-point)))
                  0
                  thresholds)
       (* points-per-flag (reduce + 0 (vals (:flags measurements)))))))

(defn rounded
  "A score rounded to hundredths."
  [value]
  (/ (Math/round (* 100.0 value)) 100.0))

(defn excesses
  "The measurements past their thresholds, and any flags, which the
  inspector lists as the reasons for a score."
  [measurements thresholds]
  (merge (into {}
               (keep (fn [[measurement {:keys [allowed]}]]
                       (let [value (get measurements measurement 0)]
                         (when (> value allowed) [measurement value]))))
               thresholds)
         (when (seq (:flags measurements)) {:flags (:flags measurements)})))
