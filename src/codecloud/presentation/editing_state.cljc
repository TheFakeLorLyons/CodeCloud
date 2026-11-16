(ns codecloud.presentation.editing-state
  "Editing one file's text from the cloud. A buffer holds the file's
  :lines, the :caret and the selection's :anchor as zero-based [line
  column] positions, and its undo history. Every change is pure, and the
  replacement it makes comes from comparing the lines before and after,
  so the host can apply it to the document."
  (:require [clojure.string :as string]
            [codecloud.model.source :as source]
            [codecloud.presentation.clojure-format :as clojure-format]
            [codecloud.presentation.highlight :as highlight]))

;; Text

(defn- code-unit [text index]
  #?(:clj (int (.charAt ^String text (int index)))
     :cljs (.charCodeAt text index)))

(defn- character-at [text index]
  (when (< -1 index (count text))
    (subs text index (inc index))))

(defn- high-surrogate? [unit] (<= 0xD800 unit 0xDBFF))

(defn- low-surrogate? [unit] (<= 0xDC00 unit 0xDFFF))

(defn- previous-boundary
  "The column one character left of `column`, stepping over both halves
  of a character outside the basic plane."
  [text column]
  (if (and (>= column 2)
           (low-surrogate? (code-unit text (dec column)))
           (high-surrogate? (code-unit text (- column 2))))
    (- column 2)
    (max 0 (dec column))))

(defn- next-boundary [text column]
  (if (and (< (inc column) (count text))
           (high-surrogate? (code-unit text column))
           (low-surrogate? (code-unit text (inc column))))
    (+ column 2)
    (min (count text) (inc column))))

(defn text-lines
  "The lines of `text` as editors count them, with one empty line for an
  empty file."
  [text]
  (let [lines (source/split-lines (source/normalize-text (or text "")))]
    (if (seq lines) lines [""])))

(defn- leading-whitespace [line]
  (re-find #"^[ \t]*" line))

(defn- whitespace-or-closer?
  "True for a character before which a typed bracket brings its partner,
  a space or a closing bracket."
  [character]
  (boolean (re-matches #"[\s)\]}]" character)))

(defn visual-column
  "Screen column of `column` in `line`, where a tab reaches the next
  multiple of `tab-size`."
  [line column tab-size]
  (loop [index 0
         visual 0]
    (if (< index (min column (count line)))
      (recur (inc index) (if (= "\t" (character-at line index))
                           (* tab-size (inc (quot visual tab-size)))
                           (inc visual)))
      visual)))

(defn column-at-visual [line visual tab-size]
  (loop [index 0]
    (if (and (< index (count line))
             (let [here (visual-column line index tab-size)
                   there (visual-column line (inc index) tab-size)]
               (<= (+ here there) (* 2 visual))))
      (recur (inc index))
      index)))

(defn expand-tabs
  "`text` starting at screen column `start` with each tab turned into the
  spaces that reach its stop, so text draws where columns count it."
  [text start tab-size]
  (if (string/includes? text "\t")
    (loop [index 0
           visual start
           expanded (transient [])]
      (if (< index (count text))
        (let [character (character-at text index)]
          (if (= "\t" character)
            (let [stop (* tab-size (inc (quot visual tab-size)))]
              (recur (inc index) stop (reduce conj! expanded (repeat (- stop visual) " "))))
            (recur (inc index) (inc visual) (conj! expanded character))))
        (apply str (persistent! expanded))))
    text))

;; Positions

(defn- clamp-position [lines [line column]]
  (let [line-index (max 0 (min line (dec (count lines))))]
    [line-index (max 0 (min column (count (nth lines line-index))))]))

(defn ordered
  "The selection's [start end], earlier position first."
  [{:keys [caret anchor]}]
  (if (neg? (compare anchor caret)) [anchor caret] [caret anchor]))

(defn selected?
  "True when the buffer selects some text."
  [{:keys [caret anchor]}]
  (not= caret anchor))

(defn- text-between [lines [from-line from-column] [to-line to-column]]
  (if (= from-line to-line)
    (subs (nth lines from-line) from-column to-column)
    (string/join "\n" (concat [(subs (nth lines from-line) from-column)]
                              (subvec lines (inc from-line) to-line)
                              [(subs (nth lines to-line) 0 to-column)]))))

(defn selected-text [{:keys [lines] :as buffer}]
  (let [[start end] (ordered buffer)]
    (text-between lines start end)))

(defn- replaced-lines
  "`lines` with the text from `from` to `to` replaced by `text`, and the
  position just after the inserted text."
  [lines [from-line from-column] [to-line to-column] text]
  (let [inserted (string/split text #"\n" -1)
        before (subs (nth lines from-line) 0 from-column)
        after (subs (nth lines to-line) to-column)
        middle (if (= 1 (count inserted))
                 [(str before (first inserted) after)]
                 (concat [(str before (first inserted))]
                         (subvec inserted 1 (dec (count inserted)))
                         [(str (peek inserted) after)]))
        end-line (+ from-line (dec (count inserted)))
        end-column (if (= 1 (count inserted))
                     (+ from-column (count (first inserted)))
                     (count (peek inserted)))]
    [(into (into (subvec lines 0 from-line) middle) (subvec lines (inc to-line)))
     [end-line end-column]]))

(defn with-edits
  "`lines` with `edits` made, each {:range [line column end-line
  end-column] :text} counted from zero, as a formatter returns them, with
  no two overlapping and each range read in the lines before any edit."
  [lines edits]
  (reduce (fn [current {[line column end-line end-column] :range :keys [text]}]
            (first (replaced-lines current (clamp-position current [line column]) (clamp-position current [end-line end-column])
                                   (source/normalize-text (str text)))))
          (vec lines)
          (sort-by :range #(compare %2 %1) edits)))

;; Buffers and history

(defn buffer [text]
  {:lines (text-lines text)
   :caret [0 0]
   :anchor [0 0]
   :goal nil
   :last-change nil
   :undo ()
   :redo ()})

(defn- snapshot-of [{:keys [lines caret anchor]}]
  {:lines lines :caret caret :anchor anchor})

(defn- remembered
  "The buffer's history with its state before a change of `kind` added,
  unless the change continues the typing just before it."
  [{:keys [caret last-change undo] :as current} kind]
  (let [undo-depth 200]
    (if (and (= :typing kind) (= :typing (:kind last-change)) (= caret (:caret last-change)))
      undo
      (take undo-depth (conj undo (snapshot-of current))))))

(defn- changed
  "The buffer after replacing its text from `from` to `to` with `text`,
  the caret after the new text, as a change of `kind`."
  [{:keys [lines] :as current} from to text kind]
  (let [[new-lines end] (replaced-lines lines from to text)]
    (assoc current
           :lines new-lines
           :caret end
           :anchor end
           :goal nil
           :undo (remembered current kind)
           :redo ()
           :last-change {:kind kind :caret end})))

(defn insert
  "The buffer with `text` in place of the selection. Typing one character
  at a time joins the run before it in the history."
  [current text kind]
  (let [[start end] (ordered current)]
    (changed current start end (source/normalize-text text) kind)))

(defn undo
  "The buffer as it was before its last change, or unchanged."
  [{:keys [undo redo] :as current}]
  (if-let [previous (first undo)]
    (merge current previous {:undo (rest undo)
                             :redo (conj redo (snapshot-of current))
                             :goal nil
                             :last-change nil})
    current))

(defn redo
  "The buffer with its last undone change made again, or unchanged."
  [{:keys [undo redo] :as current}]
  (if-let [following (first redo)]
    (merge current following {:undo (conj undo (snapshot-of current))
                              :redo (rest redo)
                              :goal nil
                              :last-change nil})
    current))

(defn rebase
  "The buffer holding `text`, which the file now holds after a change
  from elsewhere, with the caret kept as near its place as the text
  allows. Positions in the history no longer fit, so it is dropped."
  [{:keys [caret anchor] :as current} text]
  (let [lines (text-lines text)]
    (assoc current
           :lines lines
           :caret (clamp-position lines caret)
           :anchor (clamp-position lines anchor)
           :goal nil
           :last-change nil
           :undo ()
           :redo ())))

;; Comparing texts

(defn- common-prefix [first-text second-text]
  (let [limit (min (count first-text) (count second-text))]
    (loop [index 0]
      (if (and (< index limit) (= (code-unit first-text index) (code-unit second-text index)))
        (recur (inc index))
        index))))

(defn- common-suffix [first-text second-text limit]
  (loop [length 0]
    (if (and (< length limit)
             (= (code-unit first-text (- (count first-text) length 1))
                (code-unit second-text (- (count second-text) length 1))))
      (recur (inc length))
      length)))

(defn- position-in
  "Position `offset` characters into `lines` joined by newlines, counted
  from the start of line `first-line`."
  [lines first-line offset]
  (loop [line-index 0
         remaining offset]
    (let [length (count (nth lines line-index))]
      (if (and (> remaining length) (< (inc line-index) (count lines)))
        (recur (inc line-index) (- remaining length 1))
        [(+ first-line line-index) remaining]))))

(defn change-between
  "The one replacement that turns `before` lines into `after` lines, as
  {:range [line column end-line end-column] :text :replaced} in the
  coordinates of `before`, or nil when both hold the same text."
  [before after]
  (let [before-count (count before)
        after-count (count after)
        shortest (min before-count after-count)
        same-start (loop [index 0]
                     (if (and (< index shortest) (= (nth before index) (nth after index))) (recur (inc index)) index))]
    (when-not (and (= same-start before-count) (= same-start after-count))
      (let [same-end (loop [length 0]
                       (if (and (< length (- shortest same-start))
                                (= (nth before (- before-count length 1)) (nth after (- after-count length 1))))
                         (recur (inc length))
                         length))
            ;; When one side's changed lines are empty, a neighboring line
            ;; joins both, so the change has a place to stand.
            [first-line end-length] (cond
                                      (and (< same-start (- before-count same-end))
                                           (< same-start (- after-count same-end))) [same-start same-end]
                                      (pos? same-start) [(dec same-start) same-end]
                                      :else [same-start (dec same-end)])
            before-part (subvec before first-line (- before-count end-length))
            after-part (subvec after first-line (- after-count end-length))
            before-text (string/join "\n" before-part)
            after-text (string/join "\n" after-part)
            prefix (common-prefix before-text after-text)
            suffix (common-suffix before-text after-text (- (min (count before-text) (count after-text)) prefix))
            start (position-in before-part first-line prefix)
            end (position-in before-part first-line (- (count before-text) suffix))]
        {:range (into start end)
         :text (subs after-text prefix (- (count after-text) suffix))
         :replaced (subs before-text prefix (- (count before-text) suffix))}))))

;; Words

(defn- lisp? [language] (= :clojure language))

(defn- character-class
  "What a character counts as when moving by words, one of :space, :word,
  and :punctuation. Lisp names hold most punctuation."
  [character language]
  (cond
    (nil? character) nil
    (re-matches #"\s" character) :space
    (lisp? language) (if (re-matches #"[,()\[\]{}\"';@^~#`\\]" character) :punctuation :word)
    (or (re-matches #"[A-Za-z0-9_$]" character) (> (code-unit character 0) 127)) :word
    :else :punctuation))

(defn- word-end
  "Column after the next word or run of punctuation right of `column`,
  past any spaces before it."
  [line column language]
  (let [past-spaces (loop [index column]
                      (if (= :space (character-class (character-at line index) language)) (recur (inc index)) index))
        run-class (character-class (character-at line past-spaces) language)]
    (loop [index past-spaces]
      (if (and run-class (= run-class (character-class (character-at line index) language)))
        (recur (inc index))
        index))))

(defn- word-start
  "Column of the start of the word or run of punctuation left of
  `column`, past any spaces after it."
  [line column language]
  (let [past-spaces (loop [index column]
                      (if (= :space (character-class (character-at line (dec index)) language)) (recur (dec index)) index))
        run-class (character-class (character-at line (dec past-spaces)) language)]
    (loop [index past-spaces]
      (if (and run-class (pos? index) (= run-class (character-class (character-at line (dec index)) language)))
        (recur (dec index))
        index))))

;; Moving

(defn- first-text-column [line]
  (count (leading-whitespace line)))

(defn- moved-position
  "Where `motion` takes the caret, and the screen column up and down keep."
  [{:keys [lines caret goal]} motion {:keys [language tab-size page-lines]}]
  (let [[line column] caret
        text (nth lines line)
        last-line (dec (count lines))
        vertical (fn [target-line]
                   (let [wanted (or goal (visual-column text column tab-size))]
                     (cond
                       (neg? target-line) [[0 0] wanted]
                       (> target-line last-line) [[last-line (count (nth lines last-line))] wanted]
                       :else [[target-line (column-at-visual (nth lines target-line) wanted tab-size)] wanted])))]
    (case motion
      :left [(if (pos? column) [line (previous-boundary text column)] (if (pos? line) [(dec line) (count (nth lines (dec line)))] caret))]
      :right [(if (< column (count text)) [line (next-boundary text column)] (if (< line last-line) [(inc line) 0] caret))]
      :word-left [(if (pos? column) [line (word-start text column language)] (if (pos? line) [(dec line) (count (nth lines (dec line)))] caret))]
      :word-right [(if (< column (count text)) [line (word-end text column language)] (if (< line last-line) [(inc line) 0] caret))]
      :up (vertical (dec line))
      :down (vertical (inc line))
      :page-up (vertical (- line page-lines))
      :page-down (vertical (+ line page-lines))
      :line-start (let [indented (first-text-column text)]
                    [[line (if (= column indented) 0 indented)]])
      :line-end [[line (count text)]]
      :file-start [[0 0]]
      :file-end [[last-line (count (nth lines last-line))]])))

(defn move
  "The buffer with the caret moved by `motion`, extending the selection
  when `extend?`. Without `extend?`, moving left or right from a selection
  lands on its near end."
  [current motion extend? options]
  (if (and (selected? current) (not extend?) (contains? #{:left :right} motion))
    (let [[start end] (ordered current)
          landing (if (= :left motion) start end)]
      (assoc current :caret landing :anchor landing :goal nil :last-change nil))
    (let [[position goal] (moved-position current motion options)]
      (assoc current
             :caret position
             :anchor (if extend? (:anchor current) position)
             :goal (when (contains? #{:up :down :page-up :page-down} motion) goal)
             :last-change nil))))

(defn place
  "The buffer with the caret at `position`, extending the selection when
  `extend?`."
  [{:keys [lines] :as current} position extend?]
  (let [placed (clamp-position lines position)]
    (assoc current
           :caret placed
           :anchor (if extend? (:anchor current) placed)
           :goal nil
           :last-change nil)))

(defn select-range [{:keys [lines] :as current} from to]
  (assoc current
         :anchor (clamp-position lines from)
         :caret (clamp-position lines to)
         :goal nil
         :last-change nil))

(defn word-range
  "[start end] of the word or run of punctuation at `position`, or of the
  spaces there."
  [{:keys [lines]} position language]
  (let [[line column] (clamp-position lines position)
        text (nth lines line)
        here (or (character-class (character-at text column) language)
                 (character-class (character-at text (dec column)) language))
        same? #(= here (character-class (character-at text %) language))
        start (loop [index column] (if (and (pos? index) (same? (dec index))) (recur (dec index)) index))
        end (loop [index column] (if (and (< index (count text)) (same? index)) (recur (inc index)) index))]
    [[line start] [line end]]))

(defn line-range
  "[start end] of the whole line at `position`, newline included where
  there is a next line."
  [{:keys [lines]} [line _]]
  (let [line-index (max 0 (min line (dec (count lines))))]
    (if (< line-index (dec (count lines)))
      [[line-index 0] [(inc line-index) 0]]
      [[line-index 0] [line-index (count (nth lines line-index))]])))

(defn select-all [{:keys [lines] :as current}]
  (select-range current [0 0] [(dec (count lines)) (count (peek lines))]))

;; Deleting

(def ^:private pair-partners
  "The closing partner of each opening bracket and quote typed in pairs."
  {"(" ")" "[" "]" "{" "}" "\"" "\""})

(defn- empty-pair-at?
  "True when the caret stands between an opening bracket or quote and its
  partner, as one typed in a pair leaves it."
  [{:keys [lines caret]}]
  (let [[line column] caret
        text (nth lines line)
        partner (get pair-partners (character-at text (dec column)))]
    (boolean (and partner (= partner (character-at text column))))))

(defn delete
  "The buffer without its selection, or without the text between the
  caret and where `motion` would take it. Deleting left between an empty
  pair of brackets or quotes takes both."
  [current motion options]
  (cond
    (selected? current) (insert current "" :delete)

    (and (= :left motion) (empty-pair-at? current))
    (let [[line column] (:caret current)]
      (changed current [line (dec column)] [line (inc column)] "" :delete))

    :else
    (let [[position] (moved-position current motion options)
          [start end] (if (neg? (compare position (:caret current)))
                        [position (:caret current)]
                        [(:caret current) position])]
      (if (= start end)
        current
        (changed current start end "" :delete)))))

;; Typing in pairs

(defn- token-columns [tokens]
  (map vector tokens (reductions + 0 (map (comp count second) tokens))))

(defn- string-opening-column
  "The column where the string holding the token at `column` of the last
  of `token-lines` opened, or nil when the lines read hold no opening
  quote."
  [token-lines column text]
  (if (re-find #"^#?\"" text)
    column
    (some (fn [tokens]
            (when-let [[[_ piece-text] piece-column] (last (token-columns tokens))]
              (when (re-find #"^#?\"" piece-text) piece-column)))
          (rest (rseq token-lines)))))

(defn- literal-at
  "{:kind :string :column} inside a string, :column where it opened,
  {:kind :comment} inside a comment, else nil. Clojure is read from where
  a top-level form begins."
  [lines [line column] language]
  (let [lines-read-above 200 ; far enough up for a string or comment open at the caret, and cheap to read at each keystroke
        first-line (if (lisp? language) (clojure-format/form-start lines line) (max 0 (- line lines-read-above)))
        token-lines (highlight/tokenize-lines language (conj (subvec lines first-line line)
                                                             (str (subs (nth lines line) 0 column) " ")))
        [[kind text] start] (some (fn [[[_ token-text] token-column :as entry]]
                                    (when (> (+ token-column (count token-text)) column) entry))
                                  (token-columns (peek token-lines)))]
    (when (and start (< start column))
      (case kind
        :comment {:kind :comment}
        :string {:kind :string :column (or (string-opening-column token-lines start text) 0)}
        nil))))

(defn type-text
  "The buffer after typing `text`. Brackets and quotes come in pairs, wrap
  a selection, and step over their partner, while strings, comments, and
  escaped characters take text as typed."
  [{:keys [lines caret] :as current} text language]
  (let [[line column] caret
        line-text (nth lines line)
        following (character-at line-text column)
        preceding (character-at line-text (dec column))
        partner (get pair-partners text)
        literal? (delay (some? (literal-at lines caret language)))
        escaped? (= "\\" preceding)
        open-after? (or (nil? following) (whitespace-or-closer? following))
        caret-between (fn [typed]
                        (let [between [line (inc column)]]
                          (assoc typed :caret between :anchor between :last-change {:kind :typing :caret between})))]
    (cond
      (not= 1 (count text)) (insert current text :typing)

      (and partner (selected? current))
      (let [[[start-line start-column] [end-line end-column] :as selection-range] (ordered current)
            wrapped (changed current (first selection-range) (second selection-range) (str text (selected-text current) partner) :wrap)
            inner-end [end-line (if (= start-line end-line) (inc end-column) end-column)]]
        (assoc wrapped :anchor [start-line (inc start-column)] :caret inner-end
               :last-change {:kind :wrap :caret inner-end}))

      (and (= text following) (not escaped?) (not (selected? current))
           (or (and (contains? #{")" "]" "}"} text) (not @literal?))
               (and (= "\"" text) @literal?)))
      (caret-between current)

      (and partner open-after? (not escaped?) (not @literal?)
           (or (not= "\"" text) (not (and preceding (re-matches #"\w" preceding)))))
      (caret-between (insert current (str text partner) :typing))

      :else (insert current text :typing))))

;; Indenting

(defn indent-unit
  "The indentation one level adds in `lines`, a tab when tabs lead most
  indented lines, else the narrowest indentation of two to eight spaces,
  or the language's usual width when no line is indented."
  [lines language]
  (let [indents (keep #(let [leading (leading-whitespace %)] (when (seq leading) leading)) lines)
        tabbed (count (filter #(string/starts-with? % "\t") indents))
        widths (keep #(let [width (count %)] (when (<= 2 width 8) width))
                     (remove #(string/includes? % "\t") indents))]
    (cond
      (> (* 2 tabbed) (count indents)) "\t"
      (seq widths) (apply str (repeat (reduce min widths) " "))
      (= :python language) "    "
      :else "  ")))

(def ^:private openers {"(" ")" "[" "]" "{" "}"})

(defn- lisp-open-bracket
  "[character column] of the innermost bracket left open before
  `position`, read from the nearest line above opening a form at its
  first column, or nil."
  [lines [caret-line caret-column]]
  (let [form-start (or (some #(when (re-find #"^[(\[{]" (nth lines %)) %) (range caret-line -1 -1)) 0)]
    (loop [line form-start
           column 0
           stack ()
           in-string? false]
      (let [text (nth lines line)
            at-caret? (and (= line caret-line) (>= column caret-column))]
        (cond
          at-caret? (first stack)
          (>= column (count text)) (if (< line caret-line)
                                     (recur (inc line) 0 stack in-string?)
                                     (first stack))
          :else
          (let [character (character-at text column)]
            (cond
              in-string? (case character
                           "\\" (recur line (+ column 2) stack true)
                           "\"" (recur line (inc column) stack false)
                           (recur line (inc column) stack true))
              (= "\"" character) (recur line (inc column) stack true)
              (= ";" character) (recur line (count text) stack false)
              (= "\\" character) (recur line (+ column 2) stack false)
              (contains? openers character) (recur line (inc column) (conj stack [character column]) false)
              (contains? #{")" "]" "}"} character) (recur line (inc column) (rest stack) false)
              :else (recur line (inc column) stack false))))))))

(defn- new-line-indentation
  "Indentation for a line broken at the caret, and for the closing line
  when the caret sits between a pair of brackets, as [inner outer]."
  [{:keys [lines caret]} language unit]
  (let [[line column] caret
        text (nth lines line)
        before (string/trimr (subs text 0 column))
        after (string/triml (subs text column))
        current (leading-whitespace text)]
    (if (lisp? language)
      (let [[bracket bracket-column] (lisp-open-bracket lines caret)
            width (cond (nil? bracket) 0
                        (= "(" bracket) (+ bracket-column 2)
                        :else (inc bracket-column))]
        [(apply str (repeat width " ")) nil])
      (let [opens? (or (re-find #"[(\[{]$" before) (and (= :python language) (string/ends-with? before ":")))
            closes-pair? (and opens? (seq before) (seq after)
                              (= (get openers (subs before (dec (count before)))) (subs after 0 1)))]
        (cond
          closes-pair? [(str current unit) current]
          opens? [(str current unit) nil]
          :else [current nil])))))

(defn- spaces [width]
  (apply str (repeat width " ")))

(defn- lisp-break
  "The buffer with the selection replaced by a line break where cljfmt
  indents it. Inside a string the new line lines up under the opening
  quote, as docstrings do."
  [{:keys [lines] :as current}]
  (let [[[start-line start-column :as start] [end-line end-column]] (ordered current)
        literal (literal-at lines start :clojure)]
    (if (= :string (:kind literal))
      (insert current (str "\n" (spaces (:column literal))) :line-break)
      (let [before (string/trimr (subs (nth lines start-line) 0 start-column))
            after-text (subs (nth lines end-line) end-column)
            after-spaces (- (count after-text) (count (string/triml after-text)))
            above (conj (subvec lines 0 start-line) before)
            width (or (clojure-format/line-indentation (conj above (string/triml after-text)) (inc start-line))
                      (count (leading-whitespace after-text)))]
        (changed current [start-line (count before)] [end-line (+ end-column after-spaces)]
                 (str "\n" (spaces width)) :line-break)))))

(defn break-line
  "The buffer with the selection replaced by a line break indented for
  `language`. Outside Clojure, a break between a pair of brackets puts
  the closing one on a line of its own."
  [current language unit]
  (if (lisp? language)
    (lisp-break current)
    (let [[inner outer] (new-line-indentation current language unit)
          broken (insert current (str "\n" inner (when outer (str "\n" outer))) :line-break)]
      (if outer
        (let [[line] (:caret broken)
              caret [(dec line) (count inner)]]
          (assoc broken :caret caret :anchor caret :last-change {:kind :line-break :caret caret}))
        broken))))

(defn reformatted
  "The buffer holding `new-lines`, the same text reindented, as one change
  undo takes back, the caret kept on the same code. A caret left on a
  blank line moves to where cljfmt begins it."
  [{:keys [lines caret] :as current} new-lines language]
  (let [[moved-line :as moved] (clojure-format/caret-after-change lines new-lines caret)
        fill (when (and (lisp? language) (not (selected? current)) (string/blank? (nth new-lines moved-line)))
               (clojure-format/line-indentation new-lines moved-line))
        final-lines (if fill (assoc new-lines moved-line (spaces fill)) new-lines)
        final-caret (if fill [moved-line fill] moved)]
    (if (= lines final-lines)
      (assoc current :caret final-caret :anchor final-caret :goal nil)
      (assoc current
             :lines final-lines
             :caret final-caret
             :anchor final-caret
             :goal nil
             :undo (remembered current :format)
             :redo ()
             :last-change {:kind :format :caret final-caret}))))

(defn format-range
  "[first last] lines of the top-level Clojure forms the buffer's
  selection or caret touches."
  [{:keys [lines] :as current}]
  (let [[[start-line] [end-line]] (ordered current)]
    (clojure-format/top-level-lines lines start-line end-line)))

;; Matching brackets

(def ^:private bracket-partners
  {"(" ")" "#(" ")" "[" "]" "{" "}" "#{" "}"})

(defn- line-brackets [tokens]
  (loop [remaining tokens
         column 0
         found []]
    (if-let [[kind text] (first remaining)]
      (recur (rest remaining) (+ column (count text))
             (if (= :bracket kind) (conj found [text column]) found))
      found)))

(defn bracket-pair
  "{:open :open-width :close :matched?} for the bracket beside `caret`,
  the one before it first, and its partner within `reach` lines, or nil.
  Brackets of two kinds do not match."
  [token-lines [line column] reach]
  (let [here (line-brackets (get token-lines line))
        beside (or (some (fn [[text start]] (when (= column (+ start (count text))) [text start])) here)
                   (some (fn [[text start]] (when (= column start) [text start])) here))]
    (when-let [[text start] beside]
      (let [opening? (contains? bracket-partners text)
            last-line (if opening? (dec (count token-lines)) 0)
            lines-scanned (if opening?
                            (range line (min (count token-lines) (+ line reach 1)))
                            (range line (max -1 (- line reach 1)) -1))
            partner (loop [remaining lines-scanned
                           depth 0]
                      (if-let [scanned-line (first remaining)]
                        (let [brackets (cond->> (line-brackets (get token-lines scanned-line))
                                         (= scanned-line line) (filter (fn [[_ bracket-column]]
                                                                         (if opening?
                                                                           (> bracket-column start)
                                                                           (< bracket-column start))))
                                         (not opening?) reverse)
                              [found depth-after] (reduce (fn [[_ depth-now] [bracket-text bracket-column]]
                                                            (let [opens? (contains? bracket-partners bracket-text)]
                                                              (cond
                                                                (= opens? opening?) [nil (inc depth-now)]
                                                                (zero? depth-now) (reduced [[bracket-text [scanned-line bracket-column]] depth-now])
                                                                :else [nil (dec depth-now)])))
                                                          [nil depth]
                                                          brackets)]
                          (cond
                            found found
                            (= scanned-line last-line) :none
                            :else (recur (rest remaining) depth-after)))
                        :beyond-reach))]
        (case partner
          :beyond-reach nil
          :none (if opening?
                  {:open [line start] :open-width (count text) :close nil :matched? false}
                  {:open nil :open-width nil :close [line start] :matched? false})
          (let [[partner-text partner-position] partner
                [open-text open close-text close] (if opening?
                                                    [text [line start] partner-text partner-position]
                                                    [partner-text partner-position text [line start]])]
            {:open open
             :open-width (count open-text)
             :close close
             :matched? (= (get bracket-partners open-text) close-text)}))))))

(defn- selected-lines
  "Indexes of the lines a selection covers; a selection ending at the
  start of a line leaves that line out."
  [current]
  (let [[[start-line] [end-line end-column]] (ordered current)]
    (range start-line (inc (if (and (> end-line start-line) (zero? end-column)) (dec end-line) end-line)))))

(defn- edited-lines
  "The buffer with each selected line rewritten by `rewrite`, from its
  index and text to its new text or nil to keep it, keeping the selection
  on the same text."
  [{:keys [lines caret anchor] :as current} rewrite kind]
  (let [indexes (selected-lines current)
        rewritten (reduce (fn [all index]
                            (if-let [new-text (rewrite (nth all index))] (assoc all index new-text) all))
                          lines
                          indexes)
        shifted (fn [[line column]]
                  (if (some #{line} indexes)
                    [line (max 0 (+ column (- (count (nth rewritten line)) (count (nth lines line)))))]
                    [line column]))]
    (if (= rewritten lines)
      current
      (assoc current
             :lines rewritten
             :caret (shifted caret)
             :anchor (shifted anchor)
             :goal nil
             :undo (remembered current kind)
             :redo ()
             :last-change {:kind kind :caret (shifted caret)}))))

(defn indent
  "The buffer one level further in, with spaces or a tab at the caret, or
  every selected line indented when the selection spans lines."
  [{:keys [lines caret] :as current} unit tab-size]
  (let [[[start-line] [end-line]] (ordered current)]
    (if (= start-line end-line)
      (let [[line column] caret
            visual (visual-column (nth lines line) column tab-size)
            spaces (if (= "\t" unit) "\t" (apply str (repeat (- (count unit) (mod visual (count unit))) " ")))]
        (insert current spaces :indent))
      (edited-lines current #(when (seq %) (str unit %)) :indent))))

(defn outdent
  "The buffer one level further out on every selected line."
  [current unit]
  (edited-lines current
                (fn [text]
                  (let [leading (leading-whitespace text)
                        removed (cond
                                  (string/starts-with? leading "\t") 1
                                  :else (min (count leading) (count unit)))]
                    (when (pos? removed) (subs text removed))))
                :indent))

(defn toggle-comment
  "The buffer with the selected lines commented with `marker`, or
  uncommented when every one of them already is."
  [{:keys [lines] :as current} marker]
  (let [indexes (selected-lines current)
        written (remove string/blank? (map #(nth lines %) indexes))
        commented? (and (seq written)
                        (every? #(string/starts-with? (string/triml %) marker) written))
        margin (reduce min ##Inf (map #(count (leading-whitespace %)) written))]
    (edited-lines current
                  (fn [text]
                    (cond
                      (string/blank? text) nil
                      commented? (let [leading (leading-whitespace text)
                                       rest-text (subs text (+ (count leading) (count marker)))]
                                   (str leading (if (string/starts-with? rest-text " ") (subs rest-text 1) rest-text)))
                      :else (str (subs text 0 margin) marker " " (subs text margin))))
                  :comment)))
