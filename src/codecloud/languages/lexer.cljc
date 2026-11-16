(ns codecloud.languages.lexer
  "One lexer for every language, driven by a lexicon. Strings and
  comments open at a line's end continue on the next.

  A lexicon holds :spans {:open :close :escape :kind}, :regions, ordered
  :patterns [kind regex applies?], a :word-kind function, and its
  :insignificant kinds. A span without :close ends with its line. A
  region {:open :lexicon :close :open-kind :close-kind} reads with a
  lexicon of its own, and may leave its close to the outer lexicon,
  build it from the opening, or open another region on closing.")

(defn- at?
  "True when `text` starts at `index` of `line`, checked without copying
  the line, since this runs at nearly every character."
  [line index text]
  #?(:clj (.startsWith ^String line ^String text (int index))
     :cljs (.startsWith line text index)))

(defn- span-end
  "Index past the close of a span whose body starts at `start`, or nil
  when it runs past the line's end."
  [line start {:keys [close escape]}]
  (if (nil? close)
    (count line)
    (loop [index start]
      (cond
        (>= index (count line)) nil
        (and escape (at? line index escape)) (recur (+ index (count escape) 1))
        (at? line index close) (+ index (count close))
        :else (recur (inc index))))))

(defn- span-token [line start body-start span]
  (if-let [end (span-end line body-start span)]
    [end (subs line start end) false]
    [(count line) (subs line start) (not (:single-line? span))]))

(defn- opening-span [spans line position previous]
  (some (fn [span]
          (when (and (at? line position (:open span))
                     (if-let [allowed? (:allowed? span)] (allowed? previous) true))
            span))
        spans))

(defn- matched-text [found]
  (if (string? found) found (first found)))

(defn- matched-pattern [patterns remaining previous]
  (some (fn [[kind pattern applies?]]
          (when (or (nil? applies?) (applies? previous))
            (when-let [found (re-find pattern remaining)]
              [kind (matched-text found)])))
        patterns))

(defn- opening-region [regions remaining]
  (some (fn [region]
          (when-let [found (re-find (:open region) remaining)]
            (when (seq (matched-text found)) [region found])))
        regions))

(defn- with-top [frames frame]
  (assoc frames (dec (count frames)) frame))

(defn- noted
  "`frame` after a token of `kind`, which becomes its previous significant
  token, keeping the frame's first word."
  [{:keys [lexicon first-word] :as frame} kind text]
  (cond-> frame
    (and (nil? first-word) (not= :space kind)) (assoc :first-word text)
    (not (contains? (:insignificant lexicon) kind)) (assoc :previous [kind text])))

(defn- region-frame [region found surrounding-lexicon]
  {:lexicon (let [named (:lexicon region)]
             (cond
               (= :self named) surrounding-lexicon
               (fn? named) (named)
               :else named))
   :close (if-let [close-for (:close-for region)] (close-for found) (:close region))
   :close-kind (:close-kind region)
   :close-before? (:close-before? region)
   :then (:then region)
   :opening (matched-text found)})

(defn- entered [frames region found]
  (let [frame (region-frame region found (:lexicon (peek frames)))]
    (if (:next-line? region)
      (with-top frames (assoc (peek frames) :pending frame))
      (conj frames frame))))

(defn- open-pending [frames]
  (if-let [pending (:pending (peek frames))]
    (conj (with-top frames (dissoc (peek frames) :pending)) pending)
    frames))

(defn- closed
  "`frames` once the top region closes with `matched`, starting any region
  its :then names."
  [frames matched]
  (let [{:keys [close-kind close-before? then opening first-word]} (peek frames)
        outer (pop frames)
        outer (if (or close-before? (= "" matched))
                outer
                (with-top outer (noted (peek outer) close-kind matched)))]
    (if-let [following (when then (then opening first-word))]
      (entered outer following opening)
      outer)))

(defn- tokenize-line
  "[tokens frames] of one line, read with the frames the line before left
  open."
  [line frames]
  (let [line-length (count line)]
    (loop [position 0
           tokens []
           frames (open-pending frames)]
      (if (>= position line-length)
        [tokens frames]
        (let [{:keys [lexicon open-span close previous] :as frame} (peek frames)
              remaining (subs line position)]
          (if open-span
            (let [[end text still-open?] (span-token line position position open-span)]
              (recur end
                     (conj tokens [(:kind open-span) text])
                     (with-top frames (if still-open?
                                        frame
                                        (noted (dissoc frame :open-span) (:kind open-span) text)))))
            (if-let [found (when close (re-find close remaining))]
              (let [matched (matched-text found)]
                (if (or (:close-before? frame) (= "" matched))
                  (recur position tokens (closed frames matched))
                  (recur (+ position (count matched))
                         (conj tokens [(:close-kind frame) matched])
                         (closed frames matched))))
              (if-let [span (opening-span (:spans lexicon) line position previous)]
                (let [[end text still-open?] (span-token line position (+ position (count (:open span))) span)]
                  (recur end
                         (conj tokens [(:kind span) text])
                         (with-top frames (if still-open?
                                            (assoc frame :open-span span)
                                            (noted frame (:kind span) text)))))
                (if-let [[region found] (opening-region (:regions lexicon) remaining)]
                  (let [opening (matched-text found)
                        open-kind (:open-kind region)]
                    (recur (+ position (count opening))
                           (conj tokens [open-kind opening])
                           (entered (with-top frames (noted frame open-kind opening)) region found)))
                  (let [[pattern-kind matched] (or (matched-pattern (:patterns lexicon) remaining previous)
                                                   [:text (subs remaining 0 1)])
                        end (+ position (count matched))
                        kind (if (= :word pattern-kind)
                               ((:word-kind lexicon) matched previous line end)
                               pattern-kind)]
                    (recur end
                           (conj tokens [kind matched])
                           (with-top frames (noted frame kind matched)))))))))))))

(defn- inside-span?
  "True when a line read with `frames` begins inside a span or region an
  earlier line left open."
  [frames]
  (let [top (peek frames)]
    (or (some? (:open-span top)) (some? (:pending top)) (some? (:close top)))))

(defn tokenize-lines-marked
  "{:tokens :continued?} for each of `lines`, where :continued? marks a
  line beginning inside a span or region."
  [lexicon lines]
  (first
   (reduce (fn [[tokenized frames] line]
             (let [[tokens next-frames] (tokenize-line line frames)]
               [(conj tokenized {:tokens tokens :continued? (inside-span? frames)}) next-frames]))
           [[] [{:lexicon lexicon}]]
           lines)))

(defn tokenize-lines [lexicon lines]
  (mapv :tokens (tokenize-lines-marked lexicon lines)))

(defn- read-lines
  "{:tokens :starts :rest :end} of `lines` from `first-index`, stopping
  once `settled?` accepts a line and the frames it starts from."
  [lines first-index frames settled?]
  (loop [index first-index
         frames frames
         tokens (transient [])
         starts (transient [])]
    (if (or (>= index (count lines)) (and (> index first-index) (settled? index frames)))
      {:tokens (persistent! tokens) :starts (persistent! starts) :rest index :end frames}
      (let [[line-tokens next-frames] (tokenize-line (nth lines index) frames)]
        (recur (inc index) next-frames (conj! tokens line-tokens) (conj! starts frames))))))

(defn tokenized
  "Tokens of `lines` with the frames each line starts from and the frames
  after the last, kept so that after an edit only the changed lines are
  read again."
  [lexicon lines]
  (let [{:keys [tokens starts end]} (read-lines lines 0 [{:lexicon lexicon}] (constantly false))]
    {:lexicon lexicon :lines lines :tokens tokens :starts starts :end end}))

(defn retokenized
  "What `tokenized` gives for `lines`, from the result for the lines
  before an edit, reading again from the first changed line until a line
  past the edit starts from the same frames as before."
  [{:keys [lexicon tokens starts] old-lines :lines :as previous} lines]
  (let [old-count (count old-lines)
        new-count (count lines)
        shortest (min old-count new-count)
        first-changed (loop [index 0]
                        (if (and (< index shortest) (= (nth old-lines index) (nth lines index))) (recur (inc index)) index))]
    (if (and (= first-changed old-count) (= first-changed new-count))
      (assoc previous :lines lines)
      (let [same-end (loop [length 0]
                       (if (and (< length (- shortest first-changed))
                                (= (nth old-lines (- old-count length 1)) (nth lines (- new-count length 1))))
                         (recur (inc length))
                         length))
            shift (- new-count old-count)
            unchanged-from (- new-count same-end)
            settled? (fn [index frames]
                       (and (>= index unchanged-from) (= frames (nth starts (- index shift)))))
            reread (read-lines lines first-changed
                               (if (< first-changed old-count) (nth starts first-changed) (:end previous))
                               settled?)
            kept-from (min old-count (- (:rest reread) shift))]
        {:lexicon lexicon
         :lines lines
         :tokens (into (into (subvec tokens 0 first-changed) (:tokens reread)) (subvec tokens kept-from))
         :starts (into (into (subvec starts 0 first-changed) (:starts reread)) (subvec starts kept-from))
         :end (if (< (:rest reread) new-count) (:end previous) (:end reread))}))))
