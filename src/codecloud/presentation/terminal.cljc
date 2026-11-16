(ns codecloud.presentation.terminal
  "Terminal output as plain lines for a log, without the escape sequences
  for colors, cursor moves, and shell integration, with carriage returns
  redrawing a line in place, as progress bars do, and backspaces taking
  back a character. `feed` takes output a chunk at a time and hands back
  the lines it completed, and `flush-line` hands back what remains."
  (:require [clojure.string :as string]))

(def empty-state
  "What `feed` keeps between chunks before any output, with no partial
  escape sequence and no partial line."
  {:escape "" :line ""})

(def ^:private escape-sequence
  "Escape sequences a terminal reads, among them control sequences such as
  colors, strings such as titles and shell integration marks, character
  set choices, and two-character escapes such as saving the cursor."
  #"\u001b(?:\[[0-?]*[ -/]*[@-~]|[\]PX^_][^\u0007\u001b]*(?:\u0007|\u001b\\)|[()*+][0-9A-Za-z]|[0-OQ-WYZ\\`a-~])")

(def ^:private redraw-sequence
  "Control sequences that move the cursor up or switch to the alternate
  screen, which programs drawing over the terminal in place send."
  #"\u001b\[(?:\?1049h|\?47h|\d*[AF])")

(def ^:private column-mark
  "Marks the column a cursor move goes to, which the line reads once it
  knows how long it is."
  \uE000)

(defn- spacing
  "What an escape sequence leaves in the text. Moving the cursor forward
  leaves spaces and moving it to a column leaves a mark, as programs that
  place words by column write no spaces between them."
  [escape-sequence]
  (if-let [[_ amount final] (re-matches #"\u001b\[(\d*)([CG])" escape-sequence)]
    (let [amount (max 1 (or (parse-long amount) 1))]
      (if (= "C" final)
        (apply str (repeat amount " "))
        (str column-mark amount column-mark)))
    ""))

(def ^:private unfinished-escape
  "The start of an escape sequence that the text ends inside."
  #"\u001b(?:\[[0-?]*[ -/]*|[\]PX^_][^\u0007\u001b]*\u001b?|[()*+])?")

(defn- split-escape
  "`text` without its escape sequences, and an escape sequence at its end
  still missing its last characters. An escape of no known kind goes,
  leaving what follows it."
  [text]
  (let [longest-pending 4096
        cleaned (string/replace text escape-sequence spacing)
        last-escape (string/last-index-of cleaned "\u001b")
        ;; A string still waiting for its terminator may end in that
        ;; terminator's first half, so the escape before the last counts.
        start (some (fn [index] (when (and index (re-matches unfinished-escape (subs cleaned index))) index))
                    [(when (pos? (or last-escape 0)) (string/last-index-of cleaned "\u001b" (dec last-escape)))
                     last-escape])
        pending (when start (subs cleaned start))
        kept (if start (subs cleaned 0 start) cleaned)]
    (if (and pending (<= (count pending) longest-pending))
      [kept pending]
      ;; A sequence this long never ends; dropping it keeps memory bounded.
      [kept ""])))

(defn- printable-character?
  "True for the characters a log shows, which are all but control
  characters, tabs excepted."
  [character]
  (let [code #?(:clj (int character) :cljs (.charCodeAt character 0))]
    (or (= 9 code) (and (>= code 32) (not= 127 code)))))

(defn feed
  "Reads `chunk` after what `state` kept, as {:state :lines :redraws?},
  :lines every line it completed and :redraws? true when it drew over
  lines above, which plain lines cannot show."
  [{:keys [escape line]} chunk]
  (let [input (str escape chunk)
        redraws? (boolean (re-find redraw-sequence input))
        [text pending-escape] (split-escape input)
        characters (vec text)
        character-count (count characters)]
    (loop [index 0
           current line
           completed []]
      (if (>= index character-count)
        {:state {:escape pending-escape :line current} :lines (apply str completed) :redraws? redraws?}
        (let [character (nth characters index)
              following (when (< (inc index) character-count) (nth characters (inc index)))]
          (cond
            (= column-mark character)
            (let [mark-end (or (first (filter #(= column-mark (nth characters %)) (range (inc index) character-count)))
                               character-count)
                  column (dec (or (parse-long (apply str (subvec characters (inc index) mark-end))) 1))]
              (recur (inc mark-end)
                     (str current (apply str (repeat (- column (count current)) " ")))
                     completed))

            (= \newline character) (recur (inc index) "" (conj completed current "\n"))
            (and (= \return character) (contains? #{\return \newline} following)) (recur (inc index) current completed)
            (= \return character) (recur (inc index) "" completed)
            (= \backspace character) (recur (inc index) (subs current 0 (max 0 (dec (count current)))) completed)
            (printable-character? character) (recur (inc index) (str current character) completed)
            :else (recur (inc index) current completed)))))))

(defn flush-line
  "Returns {:state :lines} handing back the line `state` holds, without a
  newline, such as a prompt that waits for input."
  [{:keys [line]}]
  {:state empty-state :lines line})
