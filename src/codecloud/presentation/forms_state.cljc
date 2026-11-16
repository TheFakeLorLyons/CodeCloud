(ns codecloud.presentation.forms-state
  "The Clojure forms of a file, read from the lexer's tokens, for the
  evaluations Calva names. Positions are zero-based [line column] pairs.
  A form is a map with its :kind, one of :list, :vector, :map, :set,
  :function, and :atom, its :start, which takes in reader prefixes such
  as a quote or #?, its :end just past its last character, whether it is
  :closed?, and its :children or its :text."
  (:require [clojure.string :as string]
            [codecloud.presentation.highlight :as highlight]))

(def ^:private passed-over
  "Token kinds that hold no form, which are spacing, comments, and the
  discard mark, whose form still counts, as one written to be evaluated
  by hand."
  #{:space :comment :discard})

(def ^:private collection-kinds
  {"(" :list "[" :vector "{" :map "#{" :set "#(" :function})

(defn- positioned-tokens
  "The tokens of `lines` that hold forms, each [kind text line column]."
  [lines]
  (into []
        (comp (map-indexed (fn [line-index line-tokens]
                             (loop [column 0
                                    remaining line-tokens
                                    kept (transient [])]
                               (if-let [[kind text] (first remaining)]
                                 (recur (+ column (count text))
                                        (rest remaining)
                                        (if (contains? passed-over kind) kept (conj! kept [kind text line-index column])))
                                 (persistent! kept)))))
              cat)
        (highlight/tokenize-lines :clojure lines)))

(defn- token-start [[_kind _text line column]]
  [line column])

(defn- token-end [[_kind text line column]]
  [line (+ column (count text))])

(defn- opener? [[kind text]]
  (and (= :bracket kind) (contains? collection-kinds text)))

(defn- closer? [[kind text]]
  (and (= :bracket kind) (contains? #{")" "]" "}"} text)))

(defn- ends-string? [body]
  (boolean (re-find #"(?:^|[^\\])(?:\\\\)*\"$" body)))

(defn- read-atom
  "[form next-index] for the atom at token `index`. A string the lexer
  split across lines takes in the lines it runs on."
  [tokens index]
  (let [[kind text :as token] (get tokens index)
        atom-form (fn [end closed? next-index]
                    [{:kind :atom :text text :start (token-start token) :end end :closed? closed?} next-index])]
    (if (and (= :string kind)
             (re-find #"^#?\"" text)
             (not (ends-string? (string/replace-first text #"^#?\"" ""))))
      (loop [position (inc index)]
        (let [piece (get tokens position)]
          (cond
            (or (nil? piece) (not= :string (first piece))) (atom-form (token-end (get tokens (dec position))) false position)
            (ends-string? (second piece)) (atom-form (token-end piece) true (inc position))
            :else (recur (inc position)))))
      (atom-form (token-end token) true (inc index)))))

(defn- starts-top-level?
  "True for an opening bracket at the first column of its line, where a
  top-level form begins by convention, so a form left open above it ends
  there."
  [[_kind _text _line column :as token]]
  (and (opener? token) (zero? column)))

(defn- read-form
  "[form next-index] for the form starting at token `index`, or nil. With
  `recover?` an unclosed collection stops before a bracket at a line's
  first column, so the forms below stay whole."
  [tokens index recover?]
  (when-let [token (get tokens index)]
    (cond
      (closer? token) nil

      (= :reader (first token))
      ;; Metadata is a form of its own before the one it marks.
      (let [[target after] (if (= "^" (second token))
                             (when-let [[_metadata after-metadata] (read-form tokens (inc index) recover?)]
                               (read-form tokens after-metadata recover?))
                             (read-form tokens (inc index) recover?))]
        (if target
          [(assoc target :start (token-start token)) after]
          [{:kind :atom :text (second token) :start (token-start token) :end (token-end token) :closed? true} (inc index)]))

      (opener? token)
      (loop [children []
             position (inc index)]
        (let [next-token (get tokens position)
              collection (fn [end closed?]
                           {:kind (get collection-kinds (second token))
                            :start (token-start token)
                            :end end
                            :closed? closed?
                            :children children})]
          (cond
            (or (nil? next-token) (and recover? (starts-top-level? next-token)))
            [(collection (if (seq children) (:end (peek children)) (token-end token)) false) position]

            (closer? next-token) [(collection (token-end next-token) true) (inc position)]
            :else (let [[child after] (read-form tokens position recover?)]
                    (recur (conj children child) after)))))

      :else (read-atom tokens index))))

(defn- forms-of
  "Every top-level form of `tokens`, read as read-form reads with
  `recover?`. A closing bracket with nothing open is passed over."
  [tokens recover?]
  (let [token-count (count tokens)]
    (loop [forms []
           index 0]
      (if (< index token-count)
        (if-let [[form after] (read-form tokens index recover?)]
          (recur (conj forms form) after)
          (recur forms (inc index)))
        forms))))

(defn read-forms
  "Every top-level form of `lines`, a vector of strings. An opening
  bracket at the first column of a line begins a top-level form, ending
  any form left open above it."
  [lines]
  (forms-of (positioned-tokens lines) true))

(defn- at-or-before? [first-position second-position]
  (not (pos? (compare first-position second-position))))

(defn- touches? [{:keys [start end]} position]
  (and (at-or-before? start position) (at-or-before? position end)))

(defn- inside? [{:keys [start end] :as form} position]
  (and (touches? form position) (not= position start) (not= position end)))

(defn form-at
  "The innermost form at `position`, or the collection holding it between
  elements, or nil. Where two forms meet, the one ending there wins, as
  the caret usually follows a form just typed."
  [forms position]
  (when-let [form (first (filter #(touches? % position) forms))]
    (if (and (:children form) (inside? form position))
      (or (form-at (:children form) position) form)
      form)))

(defn- open-at
  "The collections among `forms` that hold `position` inside them, off
  their edges, outermost first."
  [forms position]
  (loop [level forms
         found []]
    (if-let [form (first (filter #(and (:children %) (inside? % position)) level))]
      (recur (:children form) (conj found form))
      found)))

(defn enclosing-at
  "The innermost collection among `forms` holding `position` inside it,
  or nil when `position` lies outside every collection."
  [forms position]
  (peek (open-at forms position)))

(defn closing-brackets
  "The brackets closing every collection among `forms` that holds
  `position` and starts at `start` or after it, innermost first, so the
  text from `start` to `position` reads whole with them after it."
  [forms start position]
  (let [closers {:list ")" :vector "]" :map "}" :set "}" :function ")"}]
    (->> (open-at forms position)
         (filter #(at-or-before? start (:start %)))
         (map (comp closers :kind))
         reverse
         (apply str))))

(defn- comment-form? [{:keys [kind children]}]
  (and (= :list kind) (= "comment" (:text (first children)))))

(defn top-level-at
  "The top-level form holding or touching `position`, or else the last
  one ending before it on its line. Inside a (comment ...) form, the
  form within it at `position`, the way a rich comment is evaluated."
  [forms position]
  (let [[line] position
        form (or (first (filter #(touches? % position) forms))
                 (last (filter (fn [{:keys [end]}] (and (= line (first end)) (at-or-before? end position))) forms)))]
    (if (and form (comment-form? form) (inside? form position))
      (or (first (filter #(touches? % position) (rest (:children form)))) form)
      form)))

(defn namespace-at
  "The namespace named for code at `position` by the last top-level ns or
  in-ns form before it, else the file's first, or nil."
  [forms position]
  (let [named (keep (fn [{:keys [kind children start]}]
                      (when (= :list kind)
                        (let [[head target] children]
                          (when (and (contains? #{"ns" "in-ns"} (:text head)) (= :atom (:kind target)))
                            [start (:text target)]))))
                    forms)]
    (second (or (last (filter #(at-or-before? (first %) position) named))
                (first named)))))

(defn namespace-form
  "The first closed (ns ...) form at the top level among `forms`, or nil."
  [forms]
  (some (fn [{:keys [kind children closed?] :as form}]
          (when (and (= :list kind) closed? (= "ns" (:text (first children))))
            form))
        forms))

(defn form-text [lines {[start-line start-column] :start [end-line end-column] :end}]
  (if (= start-line end-line)
    (subs (nth lines start-line) start-column end-column)
    (string/join "\n" (concat [(subs (nth lines start-line) start-column)]
                              (subvec lines (inc start-line) end-line)
                              [(subs (nth lines end-line) 0 end-column)]))))

(defn complete?
  "True when the forms of `lines` close every bracket and string they
  open, so the text reads as written, wherever its lines begin."
  [lines]
  (every? :closed? (forms-of (positioned-tokens lines) false)))
