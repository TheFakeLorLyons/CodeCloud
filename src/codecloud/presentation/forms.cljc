(ns codecloud.presentation.forms
  "The Clojure forms of a file, read from the lexer's tokens, for
  evaluating the form at the caret, the top-level form around it, or a
  whole file in its namespace. Positions are zero-based [line column]
  pairs. A form is a map with :kind (:list, :vector, :map, :set,
  :function, or :atom), its :start, which takes in reader prefixes such
  as a quote or #?, its :end just past its last character, whether it is
  :closed?, its :children when it is a collection, and its :text when it
  is an atom."
  (:require [clojure.string :as string]
            [codecloud.presentation.highlight :as highlight]))

(def ^:private passed-over
  "Token kinds that hold no form: spacing, comments, and the discard mark,
  whose form still counts, as one written to be evaluated by hand."
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

(defn- ends-string?
  "True when `body`, text inside a string, ends the string with a quote
  no backslash escapes."
  [body]
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

(defn- read-form
  "[form next-index] for the form starting at token `index`, or nil at a
  closing bracket or past the last token. A collection left open runs to
  its last element."
  [tokens index]
  (when-let [token (get tokens index)]
    (cond
      (closer? token) nil

      (= :reader (first token))
      ;; Metadata is a form of its own before the one it marks.
      (let [[target after] (if (= "^" (second token))
                             (when-let [[_metadata after-metadata] (read-form tokens (inc index))]
                               (read-form tokens after-metadata))
                             (read-form tokens (inc index)))]
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
            (nil? next-token) [(collection (if (seq children) (:end (peek children)) (token-end token)) false) position]
            (closer? next-token) [(collection (token-end next-token) true) (inc position)]
            :else (let [[child after] (read-form tokens position)]
                    (recur (conj children child) after)))))

      :else (read-atom tokens index))))

(defn read-forms
  "Every top-level form of `lines`, a vector of strings. A closing bracket
  with nothing open is passed over."
  [lines]
  (let [tokens (positioned-tokens lines)
        token-count (count tokens)]
    (loop [forms []
           index 0]
      (if (< index token-count)
        (if-let [[form after] (read-form tokens index)]
          (recur (conj forms form) after)
          (recur forms (inc index)))
        forms))))

(defn- at-or-before? [first-position second-position]
  (not (pos? (compare first-position second-position))))

(defn- touches?
  "True when `position` lies within `form` or at either edge of it."
  [{:keys [start end]} position]
  (and (at-or-before? start position) (at-or-before? position end)))

(defn- inside?
  "True when `position` lies within `form`, off its edges."
  [{:keys [start end] :as form} position]
  (and (touches? form position) (not= position start) (not= position end)))

(defn form-at
  "The form at `position` among `forms`: the innermost one it touches,
  or, when it sits between the elements of a collection, the collection.
  Where two forms meet, the one ending there wins, as the caret most
  often follows the form just typed. Nil away from every form."
  [forms position]
  (when-let [form (first (filter #(touches? % position) forms))]
    (if (and (:children form) (inside? form position))
      (or (form-at (:children form) position) form)
      form)))

(defn- comment-form?
  "True for a (comment ...) form, whose forms are evaluated one at a time."
  [{:keys [kind children]}]
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
  "The namespace an (ns name ...) or (in-ns 'name) form at the top level
  names for code at `position`: the last such form starting before it,
  or else the first one in the file. Nil when the file names none."
  [forms position]
  (let [named (keep (fn [{:keys [kind children start]}]
                      (when (= :list kind)
                        (let [[head target] children]
                          (when (and (contains? #{"ns" "in-ns"} (:text head)) (= :atom (:kind target)))
                            [start (:text target)]))))
                    forms)]
    (second (or (last (filter #(at-or-before? (first %) position) named))
                (first named)))))

(defn form-text
  "The text of `form` in `lines`, a vector of strings."
  [lines {[start-line start-column] :start [end-line end-column] :end}]
  (if (= start-line end-line)
    (subs (nth lines start-line) start-column end-column)
    (string/join "\n" (concat [(subs (nth lines start-line) start-column)]
                              (subvec lines (inc start-line) end-line)
                              [(subs (nth lines end-line) 0 end-column)]))))

(defn complete?
  "True when the forms of `lines` close every bracket and string they
  open, so the text reads as written."
  [lines]
  (every? :closed? (read-forms lines)))
