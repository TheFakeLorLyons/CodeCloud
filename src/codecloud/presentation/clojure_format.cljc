(ns codecloud.presentation.clojure-format
  "Clojure's indentation as cljfmt lays it out by default, which Calva's
  formatter follows. Forms such as defn, let, and when indent their
  bodies two spaces past their opening bracket, other calls line their
  arguments up under the first, and collections line their elements up
  just inside the opening bracket. Lines and columns count from zero."
  (:require [clojure.string :as string]
            [codecloud.presentation.highlight :as highlight]))

(def indentation-rules
  "cljfmt's default rules by form name. [:block n] indents the body two
  spaces once n arguments are done, unless a later argument shares their
  line; [:inner 0] always does, and [:inner 1] inside each nested form."
  {"alt!" [[:block 0]] "alt!!" [[:block 0]] "are" [[:block 2]] "as->" [[:block 2]]
   "binding" [[:block 1]] "bound-fn" [[:inner 0]] "case" [[:block 1]] "catch" [[:block 2]]
   "comment" [[:block 0]] "cond" [[:block 0]] "condp" [[:block 2]] "cond->" [[:block 1]]
   "cond->>" [[:block 1]] "def" [[:inner 0]] "defmacro" [[:inner 0]] "defmethod" [[:inner 0]]
   "defmulti" [[:inner 0]] "defn" [[:inner 0]] "defn-" [[:inner 0]] "defonce" [[:inner 0]]
   "defprotocol" [[:block 1] [:inner 1]] "defrecord" [[:block 2] [:inner 1]] "defstruct" [[:block 1]]
   "deftest" [[:inner 0]] "deftype" [[:block 2] [:inner 1]] "do" [[:block 0]] "doseq" [[:block 1]]
   "dotimes" [[:block 1]] "doto" [[:block 1]] "extend" [[:block 1]]
   "extend-protocol" [[:block 1] [:inner 1]] "extend-type" [[:block 1] [:inner 1]] "finally" [[:block 0]]
   "fn" [[:inner 0]] "for" [[:block 1]] "future" [[:block 0]] "go" [[:block 0]] "go-loop" [[:block 1]]
   "if" [[:block 1]] "if-let" [[:block 1]] "if-not" [[:block 1]] "if-some" [[:block 1]]
   "let" [[:block 1]] "letfn" [[:block 1]] "locking" [[:block 1]] "loop" [[:block 1]]
   "match" [[:block 1]] "ns" [[:block 1]] "proxy" [[:block 2] [:inner 1]] "reify" [[:inner 0] [:inner 1]]
   "struct-map" [[:block 1]] "testing" [[:block 1]] "thread" [[:block 0]] "try" [[:block 0]]
   "use-fixtures" [[:inner 0]] "when" [[:block 1]] "when-first" [[:block 1]] "when-let" [[:block 1]]
   "when-not" [[:block 1]] "when-some" [[:block 1]] "while" [[:block 1]] "with-local-vars" [[:block 1]]
   "with-open" [[:block 1]] "with-out-str" [[:block 0]] "with-precision" [[:block 1]]
   "with-redefs" [[:block 1]]})

(def ^:private named-rules
  "Rules for names no rule above lists, by pattern, as cljfmt's defaults
  give them."
  [[#"^def(?!ault)(?!late)(?!er)" [[:inner 0]]]
   [#"^with-" [[:inner 0]]]])

(defn- rules-for [head-name]
  (when head-name
    (let [unqualified (subs head-name (inc (or (string/last-index-of head-name "/") -1)))]
      (or (get indentation-rules unqualified)
          (some (fn [[pattern rules]] (when (re-find pattern unqualified) rules)) named-rules)))))

;; Reading the forms open at each line

(def ^:private symbol-kinds #{:text :special :definition})

(defn- list-bracket? [bracket]
  (contains? #{"(" "#("} bracket))

(defn- closes-string? [piece]
  (boolean (re-find #"(?:^|[^\\])(?:\\\\)*\"$" piece)))

(defn- opens-string? [text]
  (and (re-find #"^#?\"" text)
       (not (closes-string? (string/replace-first text #"^#?\"" "")))))

(defn- element [column first-in-line? head-name]
  {:column column :first-in-line? first-in-line? :name head-name})

(defn- add-element [stack new-element]
  (if (seq stack)
    (conj (pop stack) (update (peek stack) :elements conj new-element))
    stack))

(defn- step
  "The reading after one token at `column`, its open forms each a frame
  {:bracket :column :elements}."
  [{:keys [stack pending in-string? content?] :as reading} [kind text] column]
  (let [starts-form (fn [current head-name]
                      (if (pos? pending)
                        (update current :pending dec)
                        (update current :stack add-element (element column (not content?) head-name))))]
    (-> (cond
          (= :space kind) reading
          (= :comment kind) reading

          in-string? (cond-> reading (closes-string? text) (assoc :in-string? false))

          (contains? #{:reader :discard} kind)
          (if (pos? pending)
            (update reading :pending + (if (= "^" text) 2 1) -1)
            (-> reading
                (update :stack add-element (element column (not content?) nil))
                (assoc :pending (if (= "^" text) 2 1))))

          (= :bracket kind)
          (if (contains? #{")" "]" "}"} text)
            (update reading :stack #(if (seq %) (pop %) %))
            (-> (starts-form reading nil)
                (update :stack conj {:bracket text :column column :elements []})))

          (= :string kind)
          (cond-> (starts-form reading nil) (opens-string? text) (assoc :in-string? true))

          :else (starts-form reading (when (contains? symbol-kinds kind) text)))
        (cond-> (not= :space kind) (assoc :content? true)))))

;; Indentation

(defn- list-indent
  "Where an element at `index` of a list lines up, under the list's first
  argument from the second argument on, and else just inside the bracket."
  [{:keys [bracket column elements]} index]
  (if (and (> index 1) (> (count elements) 1))
    (:column (nth elements 1))
    (+ column (count bracket))))

(defn- inner-indent [{:keys [bracket column]}]
  (+ column (count bracket) 1))

(defn- block-indent
  "Where element `index` of a [:block `argument-count`] list goes, two
  spaces in once those arguments are done, else lined up as a call."
  [frame index argument-count]
  (let [after (get (:elements frame) (inc argument-count))]
    (if (and (or (nil? after) (:first-in-line? after)) (> index argument-count))
      (inner-indent frame)
      (list-indent frame index))))

(defn- indentation
  "The column a line goes to when its first form is the next element of
  the innermost open form in `stack`, or 0 at the top level."
  [stack]
  (if-let [frame (peek stack)]
    (if (list-bracket? (:bracket frame))
      (let [index (count (:elements frame))
            parent (peek (pop stack))
            own (some (fn [[kind amount]]
                        (case kind
                          :block (block-indent frame index amount)
                          :inner (when (zero? amount) (inner-indent frame))
                          nil))
                      (rules-for (:name (first (:elements frame)))))
            inherited (when (and parent (list-bracket? (:bracket parent)))
                        (some (fn [[kind amount]]
                                (when (and (= :inner kind) (= 1 amount)) (inner-indent frame)))
                              (rules-for (:name (first (:elements parent))))))]
        (or own inherited (list-indent frame index)))
      (+ (:column frame) (count (:bracket frame))))
    0))

(defn- leading-width [line]
  (count (re-find #"^[ \t]*" line)))

(defn- reindented-line
  "`line` begun at `width` columns, its trailing whitespace trimmed; an
  empty line stays empty."
  [line width]
  (let [content (string/trim line)]
    (if (= "" content) "" (str (apply str (repeat width " ")) content))))

(def ^:private fresh-reading
  {:stack [] :pending 0 :in-string? false :content? false})

(defn- read-line-tokens
  "The reading after the `tokens` of one line, their columns moved by
  `shift` past a leading space, which a re-indented line replaces."
  [reading tokens shift]
  (let [columns (reductions + 0 (map (comp count second) tokens))]
    (reduce (fn [current [[kind :as token] column]]
              (if (and (= :space kind) (zero? column))
                current
                (step current token (+ column shift))))
            (assoc reading :content? false)
            (map vector tokens columns))))

(defn reindent
  "`lines` with lines `from` to `to` indented as cljfmt does by default
  and trimmed, reading forms from line `start`. A line starting inside a
  string keeps its text."
  [lines start from to]
  (let [token-lines (highlight/tokenize-lines :clojure (subvec lines start (inc to)))]
    (loop [line start
           result lines
           reading fresh-reading]
      (if (> line to)
        result
        (let [original (nth lines line)
              reindent? (and (>= line from) (not (:in-string? reading)))
              written (if reindent? (reindented-line original (indentation (:stack reading))) original)]
          (recur (inc line)
                 (if (= written original) result (assoc result line written))
                 (read-line-tokens reading (nth token-lines (- line start))
                                   (- (leading-width written) (leading-width original)))))))))

(defn form-start
  "The line where the top-level form holding line `line` begins, the
  nearest line at or above it opening a form at its first column, or the
  first line."
  [lines line]
  (or (some #(when (re-find #"^(?:#\??@?)?[(\[{]" (nth lines %)) %) (range line -1 -1)) 0))

(defn- form-line-spans
  "[first last] lines of each top-level collection in `token-lines`, read
  by bracket depth from the first line, or nil when a bracket is left
  open at the end."
  [token-lines]
  (loop [line 0
         depth 0
         opened-at nil
         spans []]
    (if (= line (count token-lines))
      (when (zero? depth) spans)
      (let [[depth-after opened-after spans-after]
            (reduce (fn [[depth-now opened spans-now] [kind text]]
                      (cond
                        (not= :bracket kind) [depth-now opened spans-now]
                        (contains? #{")" "]" "}"} text) (cond
                                                          (zero? depth-now) [0 opened spans-now]
                                                          (= 1 depth-now) [0 nil (conj spans-now [opened line])]
                                                          :else [(dec depth-now) opened spans-now])
                        (zero? depth-now) [1 line spans-now]
                        :else [(inc depth-now) opened spans-now]))
                    [depth opened-at spans]
                    (nth token-lines line))]
        (recur (inc line) depth-after opened-after spans-after)))))

(defn top-level-lines
  "[first last] lines of the top-level forms that lines `from` to `to`
  touch. When brackets do not balance, a line opening a form at its first
  column begins one."
  [lines from to]
  (if-let [spans (form-line-spans (highlight/tokenize-lines :clojure lines))]
    (let [touched (filter (fn [[first-line last-line]] (and (<= first-line to) (>= last-line from))) spans)]
      [(apply min from (map first touched)) (apply max to (map second touched))])
    (let [top-level-start? #(re-find #"^(?:#\??@?)?[(\[{]" (nth lines %))]
      [(form-start lines from)
       (or (some #(when (top-level-start? %) (dec %)) (range (inc to) (count lines)))
           (dec (count lines)))])))

(defn line-indentation
  "The column line `line` of `lines` begins at under cljfmt's rules, read
  from the lines above as they stand, or nil for a line starting inside
  a string."
  [lines line]
  (let [start (form-start lines (max 0 (dec line)))
        token-lines (highlight/tokenize-lines :clojure (subvec lines start line))
        reading (reduce (fn [current tokens] (read-line-tokens current tokens 0)) fresh-reading token-lines)]
    (cond
      (zero? line) 0
      (:in-string? reading) nil
      :else (indentation (:stack reading)))))

;; Keeping the caret

(defn- whitespace? [character]
  (boolean (re-matches #"\s" character)))

(defn- offset-past
  "The offset in `text` just past its `kept`th character that is not
  whitespace, or its start when `kept` is zero."
  [text kept]
  (loop [index 0
         seen 0]
    (cond
      (= seen kept) index
      (>= index (count text)) index
      (whitespace? (subs text index (inc index))) (recur (inc index) seen)
      :else (recur (inc index) (inc seen)))))

(defn- position-at [text offset]
  (let [head (subs text 0 offset)
        line-start (inc (or (string/last-index-of head "\n") -1))]
    [(count (filter #(= \newline %) head)) (- offset line-start)]))

(defn caret-after-change
  "Where the caret at `position` in `before` belongs in `after`, the same
  text with only whitespace changed. It keeps beside the same character,
  or on a blank line, the same distance below the code above."
  [before after [line column]]
  (let [text-before (string/join "\n" before)
        text-after (string/join "\n" after)
        offset (+ (reduce + 0 (map #(inc (count %)) (take line before))) column)
        kept (count (remove #(whitespace? (str %)) (subs text-before 0 offset)))
        past-kept (offset-past text-after kept)
        line-text (nth before line)]
    (cond
      (not (string/blank? (subs line-text 0 column)))
      (position-at text-after past-kept)

      (string/blank? line-text)
      (let [lines-down (count (filter #(= \newline %) (subs text-before (offset-past text-before kept) offset)))
            [code-line] (position-at text-after past-kept)
            target-line (min (dec (count after)) (+ code-line lines-down))]
        [target-line (min column (count (nth after target-line)))])

      :else
      (position-at text-after (or (some #(when-not (whitespace? (subs text-after % (inc %))) %)
                                        (range past-kept (count text-after)))
                                  (count text-after))))))
