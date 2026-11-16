(ns codecloud.presentation.markdown
  "Markdown as the assistant writes it, read into hiccup. Every piece of
  text stays text, so nothing an answer says becomes markup of its own,
  and links lead only to web pages over HTTP or HTTPS. A fence left
  open, as in a reply still streaming, runs to the end."
  (:require [clojure.string :as string]))

;; Blocks

(def ^:private fence-pattern
  "A line opening fenced code, with backticks and an info string free of
  them, or with tildes."
  #"^ {0,3}(?:(`{3,})([^`]*)|(~{3,})(.*))$")

(def ^:private closing-fence-pattern #"^ {0,3}(`{3,}|~{3,})[ \t]*$")

(def ^:private heading-pattern #"^ {0,3}(#{1,6})[ \t]+(.*?)(?:[ \t]+#+)?[ \t]*$")

(def ^:private rule-pattern #"^ {0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})$")

(def ^:private item-pattern #"^( *)([-*+]|\d{1,9}[.)])[ \t]+(.*)$")

(def ^:private quote-pattern #"^ {0,3}>[ ]?(.*)$")

(def ^:private table-separator-pattern #"^[ \t]*\|?[ \t]*:?-+:?[ \t]*(?:\|[ \t]*:?-+:?[ \t]*)*\|?[ \t]*$")

(defn- parse-number [digits]
  #?(:clj (Long/parseLong digits) :cljs (js/parseInt digits 10)))

(defn- indentation [line]
  (count (re-find #"^ *" line)))

(defn- dedented [line width]
  (subs line (min width (indentation line))))

(defn- fence-of [line]
  (when-let [[_ backticks backtick-info tildes tilde-info] (re-matches fence-pattern line)]
    (let [language (first (string/split (string/trim (or backtick-info tilde-info "")) #"\s+"))]
      {:fence (or backticks tildes) :language (when (seq language) language)})))

(defn- heading-of [line]
  (when-let [[_ hashes text] (re-matches heading-pattern line)]
    {:kind :heading :level (count hashes) :text text}))

(defn- item-of
  "The list item `line` begins, {:indent :ordered? :number :content-indent
  :text}, else nil. The content indent is the column the item's text
  starts at, where its continuation lines line up."
  [line]
  (when-let [[_ spaces marker text] (re-matches item-pattern line)]
    (let [ordered? (not (contains? #{"-" "*" "+"} marker))]
      {:indent (count spaces)
       :ordered? ordered?
       :number (when ordered? (parse-number (subs marker 0 (dec (count marker)))))
       :content-indent (if (seq text) (- (count line) (count text)) (+ (count spaces) (count marker) 1))
       :text text})))

(defn- table-start?
  "True when the line at `at` heads a table, holding a bar with a line of
  dashes and bars after it."
  [lines at]
  (let [separator (get lines (inc at))]
    (boolean (and separator
                  (string/includes? (nth lines at) "|")
                  (string/includes? separator "|")
                  (re-matches table-separator-pattern separator)))))

(defn- starts-block?
  "True when `line` begins a block other than a paragraph, ending the
  paragraph or list item above it."
  [line]
  (boolean (or (fence-of line) (heading-of line) (re-matches rule-pattern line)
               (re-matches quote-pattern line) (item-of line))))

(declare blocks)

(defn- fenced-code
  "[block next] for the fenced code opening at `at`, which runs to its
  closing fence or to the end."
  [lines at]
  (let [{:keys [fence language]} (fence-of (nth lines at))
        indent (indentation (nth lines at))
        closes? (fn [line]
                  (when-let [[_ closing] (re-matches closing-fence-pattern line)]
                    (and (= (subs closing 0 1) (subs fence 0 1)) (>= (count closing) (count fence)))))
        end (or (some #(when (closes? (nth lines %)) %) (range (inc at) (count lines)))
                (count lines))]
    [{:kind :code
      :language language
      :text (string/join "\n" (map #(dedented % indent) (subvec lines (inc at) end)))}
     (min (count lines) (inc end))]))

(defn- quote-block [lines at]
  (let [end (or (some #(when-not (re-matches quote-pattern (nth lines %)) %) (range at (count lines)))
                (count lines))]
    [{:kind :quote
      :blocks (blocks (mapv #(second (re-matches quote-pattern %)) (subvec lines at end)))}
     end]))

(defn- table-cells
  "The cells of a table row, without its outer bars, a bar written \\| kept."
  [line]
  (let [trimmed (string/trim line)
        opened (if (string/starts-with? trimmed "|") (subs trimmed 1) trimmed)
        inner (if (and (string/ends-with? opened "|") (not (string/ends-with? opened "\\|")))
                (subs opened 0 (dec (count opened)))
                opened)]
    (mapv #(string/replace (string/trim %) "\\|" "|") (string/split inner #"(?<!\\)\|" -1))))

(defn- alignment [cell]
  (cond
    (and (string/starts-with? cell ":") (string/ends-with? cell ":")) "center"
    (string/ends-with? cell ":") "right"
    (string/starts-with? cell ":") "left"
    :else nil))

(defn- table-block
  "[block next] for the table headed at `at`, every row as wide as its
  header."
  [lines at]
  (let [header (table-cells (nth lines at))
        width (count header)
        fitted (fn [cells] (vec (take width (concat cells (repeat "")))))
        end (or (some #(let [line (nth lines %)]
                         (when (or (string/blank? line) (not (string/includes? line "|"))) %))
                      (range (+ at 2) (count lines)))
                (count lines))]
    [{:kind :table
      :alignments (fitted (map alignment (table-cells (nth lines (inc at)))))
      :header header
      :rows (mapv #(fitted (table-cells %)) (subvec lines (+ at 2) end))}
     end]))

(defn- first-filled [lines from]
  (some #(when-not (string/blank? (nth lines %)) %) (range from (count lines))))

(defn- list-block
  "[block next] for the list starting at `at`. An item holds the lines
  indented under it as blocks of their own, and a numbered item ends a
  list of bullets, as a bullet ends a numbered list."
  [lines at]
  (let [{list-indent :indent :as first-item} (item-of (nth lines at))
        ;; An item indented one space more than the first still sits beside it.
        sibling-slack 1
        sibling? (fn [item] (and item
                                 (<= (:indent item) (+ list-indent sibling-slack))
                                 (= (:ordered? item) (:ordered? first-item))))
        add-line (fn [items line] (conj (pop items) (update (peek items) :lines conj line)))
        finished (fn [items]
                   {:kind :list
                    :ordered? (:ordered? first-item)
                    :start (:number first-item)
                    :items (mapv #(blocks (:lines %)) items)})]
    (loop [index (inc at)
           items [(assoc first-item :lines [(:text first-item)])]
           after-blank? false]
      (if (>= index (count lines))
        [(finished items) index]
        (let [line (nth lines index)
              item (item-of line)]
          (cond
            (sibling? item)
            (recur (inc index) (conj items (assoc item :lines [(:text item)])) false)

            (string/blank? line)
            (let [next-filled (first-filled lines index)]
              (if (and next-filled (or (> (indentation (nth lines next-filled)) list-indent)
                                       (sibling? (item-of (nth lines next-filled)))))
                (recur (inc index) (add-line items "") true)
                [(finished items) index]))

            (> (indentation line) list-indent)
            (recur (inc index) (add-line items (dedented line (:content-indent (peek items)))) false)

            (and (not after-blank?) (not (starts-block? line)) (not (table-start? lines index)))
            (recur (inc index) (add-line items (string/trim line)) false)

            :else [(finished items) index]))))))

(defn- paragraph
  "[block next] for the paragraph beginning at `at`, its lines kept apart."
  [lines at]
  (let [end (or (some #(let [line (nth lines %)]
                         (when (or (string/blank? line) (starts-block? line) (table-start? lines %)) %))
                      (range (inc at) (count lines)))
                (count lines))]
    [{:kind :paragraph :text (string/join "\n" (map string/trim (subvec lines at end)))}
     end]))

(defn- blocks
  "The blocks of `lines`, each a map whose :kind, :heading, :paragraph,
  :code, :list, :quote, :table, or :rule, says what else it holds; list
  items and quotes hold blocks of their own."
  [lines]
  (loop [at 0
         found []]
    (if (>= at (count lines))
      found
      (let [line (nth lines at)
            [block next-at] (cond
                           (string/blank? line) [nil (inc at)]
                           (fence-of line) (fenced-code lines at)
                           (heading-of line) [(heading-of line) (inc at)]
                           (re-matches rule-pattern line) [{:kind :rule} (inc at)]
                           (re-matches quote-pattern line) (quote-block lines at)
                           (item-of line) (list-block lines at)
                           (table-start? lines at) (table-block lines at)
                           :else (paragraph lines at))]
        (recur next-at (cond-> found block (conj block)))))))

(defn read-blocks
  "The blocks of Markdown `text`, as `blocks` describes them, with tabs
  read as four spaces."
  [text]
  (blocks (vec (string/split-lines (string/replace (str text) "\t" "    ")))))

;; Inline marks

(def ^:private reference-pattern
  "A web address, or a word that may name a file, with a line or a range
  of lines after a colon."
  #"https?://[^\s<>()\[\]`]*[^\s<>()\[\]`.,;:!?'\"*_]|(?:\.{0,2}/)?(?:[\w.\-]+/)*[\w.\-]*\w\.[A-Za-z0-9]+(?::\d+(?:[-–]\d+)?)?")

(def ^:private link-pattern #"^\[([^\]\n]+)\]\(\s*<?([^\s()<>]+)>?(?:\s+\"[^\"\n]*\")?\s*\)")

(def ^:private web-address-pattern #"^https?://\S+")

(defn- next-match [pattern text from]
  #?(:clj (let [matcher (re-matcher pattern text)]
            (when (.find matcher (int from))
              [(.start matcher) (.group matcher)]))
     :cljs (when-let [found (.exec pattern (subs text from))]
             [(+ from (.-index found)) (aget found 0)])))

(defn- web-link [address children]
  (into [:a {:href address :target "_blank" :rel "noopener noreferrer" :title address}] children))

(defn- place-of [reference {:keys [place-for place-link]}]
  (when (and place-for place-link)
    (place-for reference)))

(defn- referenced
  "Hiccup for a line of plain `text`, the web addresses and places it
  names made links."
  [text {:keys [place-link] :as options}]
  (loop [from 0
         nodes []]
    (if-let [[start matched] (next-match reference-pattern text from)]
      (let [end (+ start (count matched))
            place (when-not (re-find web-address-pattern matched) (place-of matched options))
            link (cond
                   (re-find web-address-pattern matched) (web-link matched [matched])
                   place (place-link place [matched])
                   :else nil)]
        (recur end (if link
                     (conj nodes (subs text from start) link)
                     (conj nodes (subs text from end)))))
      (conj nodes (subs text from)))))

(defn- plain
  "Hiccup for plain `text`, each line break kept."
  [text options]
  (if (empty? text)
    []
    (->> (string/split text #"\n" -1)
         (map #(referenced % options))
         (interpose [[:br]])
         (apply concat)
         vec)))

(defn- merged
  "`nodes` with neighboring strings joined and empty ones gone."
  [nodes]
  (reduce (fn [joined node]
            (cond
              (= "" node) joined
              (and (string? node) (string? (peek joined))) (conj (pop joined) (str (peek joined) node))
              :else (conj joined node)))
          []
          nodes))

(defn- whitespace? [character]
  (boolean (and character (re-matches #"\s" (str character)))))

(defn- word-character? [character]
  (boolean (and character (re-matches #"\w" (str character)))))

(declare inline)

(defn- code-span
  "[node end] for the inline code whose backticks begin at `position`, or
  nil when they never close. Code naming a place links to it."
  [text position options]
  (let [run (re-find #"^`+" (subs text position))
        start (+ position (count run))
        close (string/index-of text run start)]
    (when close
      (let [content (subs text start close)
            content (if (and (> (count content) 2) (string/starts-with? content " ") (string/ends-with? content " ")
                             (not (string/blank? content)))
                      (subs content 1 (dec (count content)))
                      content)
            place (place-of (string/trim content) options)]
        [(if place ((:place-link options) place [[:code content]]) [:code content])
         (+ close (count run))]))))

(defn- link
  "[node end] for the link written at `position`, or nil. A link to a web
  page stays one, a link to a place in the codebase goes there, and any
  other link shows its words alone."
  [text position options]
  (when-let [[matched label target] (re-find link-pattern (subs text position))]
    (let [children (inline label options)
          as-line (string/replace target #"#L(\d+)(?:-L?(\d+))?$"
                                  (fn [[_ line end-line]] (str ":" line (when end-line (str "-" end-line)))))
          place (when-not (re-find web-address-pattern target) (place-of as-line options))]
      [(cond
         (re-find web-address-pattern target) (web-link target children)
         place ((:place-link options) place children)
         :else (into [:span {:title target}] children))
       (+ position (count matched))])))

(defn- closing-marker
  "Where `marker` closes the emphasis opened before `from`, which is after
  something other than space, outside a double mark for a single one, and
  outside a word for an underscore. Nil when it never does."
  [text marker from]
  (let [single? (= 1 (count marker))
        character (subs marker 0 1)]
    (loop [search from]
      (when-let [found (string/index-of text marker search)]
        (let [before (get text (dec found))
              after (get text (+ found (count marker)))]
          (if (and (> found from)
                   (not (whitespace? before))
                   (or (not single?) (and (not= character (str before)) (not= character (str after))))
                   (or (not= "_" character) (not (word-character? after))))
            found
            (recur (inc found))))))))

(defn- emphasis
  "[node end] for the emphasis, strong emphasis, or struck text opening
  at `position`, or nil. An underscore inside a word opens nothing."
  [text position options]
  (let [character (str (get text position))
        doubled? (= character (str (get text (inc position))))
        marker (if doubled? (str character character) character)
        start (+ position (count marker))
        after (get text start)
        before (when (pos? position) (get text (dec position)))]
    (when (and after
               (not (whitespace? after))
               (or (not= "~" character) doubled?)
               (or (not= "_" character) (not (word-character? before))))
      (when-let [close (closing-marker text marker start)]
        (let [tag (case marker ("**" "__") :strong ("*" "_") :em "~~" :del)]
          [(into [tag] (inline (subs text start close) options)) (+ close (count marker))])))))

(defn- mark-at
  "[node end] for the inline mark at `position`, or nil when none begins
  there."
  [text position options]
  (case (str (get text position))
    "\\" (let [following (get text (inc position))]
           (when (and following (re-matches #"[!-/:-@\[-`{-~]" (str following)))
             [(str following) (+ position 2)]))
    "`" (code-span text position options)
    "[" (link text position options)
    ("*" "_" "~") (emphasis text position options)
    nil))

(defn- inline
  "Hiccup nodes for `text` with its inline marks read."
  [text options]
  (loop [position 0
         plain-start 0
         nodes []]
    (if (>= position (count text))
      (merged (into nodes (plain (subs text plain-start) options)))
      (if-let [[node end] (mark-at text position options)]
        (recur end end (-> nodes (into (plain (subs text plain-start position) options)) (conj node)))
        (recur (inc position) plain-start nodes)))))

;; Hiccup

(declare block-hiccup)

(defn- item-hiccup [item-blocks options]
  (let [[only & others] item-blocks]
    (if (and only (nil? others) (= :paragraph (:kind only)))
      (into [:li] (inline (:text only) options))
      (into [:li] (map #(block-hiccup % options)) item-blocks))))

(defn- cell-hiccup [tag aligned text options]
  (into [tag (if aligned {:style {"text-align" aligned}} {})] (inline text options)))

(defn- block-hiccup [{:keys [kind] :as block} options]
  (case kind
    :heading (into [(if (<= (:level block) 2) :h3.md-heading :h4.md-heading)] (inline (:text block) options))
    :paragraph (into [:p] (inline (:text block) options))
    :code [:pre.md-code (if (:language block) {:data-language (:language block)} {}) [:code (:text block)]]
    :rule [:hr]
    :quote (into [:blockquote] (map #(block-hiccup % options)) (:blocks block))
    :list (let [{:keys [ordered? start items]} block]
            (into [(if ordered? :ol :ul) (if (and ordered? start (not= 1 start)) {:start start} {})]
                  (map #(item-hiccup % options))
                  items))
    :table (let [{:keys [alignments header rows]} block]
             [:div.md-table
              [:table
               [:thead (into [:tr] (map-indexed #(cell-hiccup :th (nth alignments %1) %2 options)) header)]
               (into [:tbody]
                     (map (fn [row] (into [:tr] (map-indexed #(cell-hiccup :td (nth alignments %1) %2 options)) row)))
                     rows)]])))

(defn hiccup
  "Hiccup for Markdown `text`. With :place-for and :place-link in
  `options`, references such as src/core.clj:12 become links; without
  them they stay text."
  [text options]
  (mapv #(block-hiccup % options) (read-blocks text)))
