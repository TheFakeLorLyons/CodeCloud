(ns codecloud.languages.structure
  "What scanned languages share in finding definitions, top-level spans,
  and complection, from tokens and brackets to measuring a span, the items
  of a braced body, and reading a file's facts. Each language finds its
  own structure in its own namespace. Nesting counts where Clojure would
  open a list, so scores compare across languages."
  (:require [clojure.string :as string]
            [codecloud.languages.plain :as plain]
            [codecloud.languages.scanner :as scanner]
            [codecloud.model.complection :as complection]
            [codecloud.model.source :as source]))

;; Tokens

(defn- punctuation? [text]
  (boolean (re-matches #"[^\w\s\"'`$]+" text)))

(defn merged-operators
  "Significant tokens with runs of adjacent punctuation joined into one
  operator, such as =>, &&, or ?., so rules can name operators whole."
  [tokens]
  (reduce (fn [merged {:keys [kind text line column] :as token}]
            (let [previous (peek merged)]
              (if (and previous
                       (= :text kind (:kind previous))
                       (not (:continues? token))
                       (punctuation? text)
                       (punctuation? (:text previous))
                       (= line (:line previous))
                       (= column (+ (:column previous) (count (:text previous)))))
                (conj (pop merged) (assoc previous :text (str (:text previous) text)))
                (conj merged token))))
          []
          tokens))

(defn opening? [{:keys [kind text]}]
  (and (= :bracket kind) (contains? #{"(" "[" "{"} text)))

(defn- closing? [{:keys [kind text]}]
  (and (= :bracket kind) (contains? #{")" "]" "}"} text)))

(defn depths
  "How many openers are open before each token, a closer standing at the
  depth of the opener it closes."
  [opener? closer? tokens]
  (first (reduce (fn [[levels depth] token]
                   (let [here (if (closer? token) (max 0 (dec depth)) depth)]
                     [(conj levels here) (if (opener? token) (inc here) here)]))
                 [[] 0]
                 tokens)))

(defn bracket-depths [tokens]
  (depths opening? closing? tokens))

(defn matching-close
  "Index of the bracket that closes the one opened at `open-index`, or the
  last index when the file ends first."
  [tokens depths open-index]
  (let [depth (nth depths open-index)]
    (or (some #(when (and (closing? (nth tokens %)) (= depth (nth depths %))) %)
              (range (inc open-index) (count tokens)))
        (dec (count tokens)))))

(defn name-like? [token]
  (and token
       (contains? #{:text :definition :constant :keyword} (:kind token))
       (re-matches #"[A-Za-z_$][\w$]*" (:text token))))

(defn- chain-at
  "[dotted-name end] for the chain of names joined by dots, such as
  np.hanning, starting at `index`, or nil when none starts there."
  [tokens index]
  (when (and (name-like? (get tokens index))
             (not (contains? #{"." "?."} (:text (get tokens (dec index))))))
    (loop [end (inc index)
           parts [(:text (get tokens index))]]
      (if (and (= "." (:text (get tokens end))) (name-like? (get tokens (inc end))))
        (recur (+ 2 end) (conj parts (:text (get tokens (inc end)))))
        [(string/join "." parts) end]))))

(defn chains
  "Every chain of names among the tokens from `start` to `end`, as
  {:name :index :called?}."
  [tokens start end]
  (loop [index start
         found []]
    (if (>= index end)
      found
      (if-let [[dotted chain-end] (chain-at tokens index)]
        (recur chain-end (conj found {:name dotted :index index :called? (= "(" (:text (get tokens chain-end)))}))
        (recur (inc index) found)))))

;; Measuring a definition

(def rules
  "What adds complection in each language, from its :decisions,
  :decision-operators, and :decision-directives to its :asynchronous
  words and calls and its :state changes."
  {:python {:decisions #{"if" "elif" "for" "while" "except" "case"}
            :decision-operators #{}
            :asynchronous-words #{"await"}
            :asynchronous-operators #{}
            :asynchronous-calls #{"create_task" "gather" "ensure_future" "run_in_executor" "run_coroutine_threadsafe"
                                  "call_soon" "call_later" "Thread" "Process" "ThreadPoolExecutor"
                                  "ProcessPoolExecutor" "submit"}
            :state-words #{"global" "nonlocal" "del"}
            :state-calls #{"setattr" "delattr"}}
   :javascript {:decisions #{"if" "for" "while" "case" "catch"}
                :decision-operators #{"?"}
                :asynchronous-words #{"await"}
                :asynchronous-operators #{}
                :asynchronous-calls #{"then" "setTimeout" "setInterval" "requestAnimationFrame" "queueMicrotask"
                                      "fetch" "Promise" "Promise.all" "Promise.race" "Promise.allSettled"
                                      "Promise.any" "Worker" "postMessage"}
                :state-words #{"delete"}
                :state-calls #{"Object.assign" "Object.defineProperty" "Reflect.set" "Reflect.deleteProperty"}}
   :css {:decisions #{"@if" "@each" "@for" "@while"}
         :decision-operators #{}
         :asynchronous-words #{}
         :asynchronous-operators #{}
         :asynchronous-calls #{}
         :state-words #{}
         :state-calls #{}}
   :shell {:decisions #{"if" "elif" "for" "while" "until" "select"}
           :decision-operators #{"&&" "||" ";;"}
           :asynchronous-words #{"wait" "nohup" "disown" "coproc" "parallel"}
           :asynchronous-operators #{"&"}
           :asynchronous-calls #{}
           :state-words #{"export" "unset" "cd" "pushd" "popd"}
           :state-calls #{}}
   ;; Preprocessor conditionals inside a body branch what code exists, and
   ;; locks and atomics change state other threads share.
   :c {:decisions #{"if" "for" "while" "case" "catch"}
       :decision-operators #{"?"}
       :decision-directives #{"#if" "#ifdef" "#ifndef" "#elif" "#elifdef" "#elifndef"}
       :asynchronous-words #{"co_await" "co_yield" "co_return" "thread" "jthread"}
       :asynchronous-operators #{}
       :asynchronous-calls #{"async" "pthread_create" "thrd_create" "detach"}
       :state-words #{"static" "volatile" "mutable" "thread_local" "const_cast"}
       :state-calls #{"lock" "unlock" "store" "exchange" "fetch_add" "fetch_sub"
                      "compare_exchange_weak" "compare_exchange_strong"}}})

(defn- last-segment [dotted]
  (if-let [dot-index (string/last-index-of dotted ".")]
    (subs dotted (inc dot-index))
    dotted))

(defn- chains-between
  "Those of `name-chains`, in token order, that start from token `start`
  up to `end`."
  [name-chains start end]
  (let [first-from (fn [index]
                     (loop [low 0
                            high (count name-chains)]
                       (if (< low high)
                         (let [middle (quot (+ low high) 2)]
                           (if (< (:index (nth name-chains middle)) index)
                             (recur (inc middle) high)
                             (recur low middle)))
                         low)))]
    (subvec name-chains (first-from start) (first-from end))))

(defn- word-count
  "How many of the tokens are one of `words` used as a word, which leaves
  out a member such as .catch, or one of `operators`."
  [tokens start end words operators]
  (count (filter (fn [index]
                   (let [{:keys [kind text]} (nth tokens index)]
                     (or (and (contains? words text)
                              (contains? #{:special :keyword :text :constant} kind)
                              (not= "." (:text (get tokens (dec index)))))
                         (and (= :text kind) (contains? operators text)))))
                 (range start end))))

(defn- decision-count
  "How many reserved decision words, decision operators, and conditional
  directives the tokens hold."
  [{:keys [decisions decision-operators decision-directives]} tokens start end]
  (count (filter (fn [index]
                   (let [{:keys [kind text]} (nth tokens index)]
                     (or (and (= :special kind) (contains? decisions text))
                         (and (= :text kind) (contains? decision-operators text))
                         (and (= :reader kind) (contains? decision-directives (string/replace text #"\s+" ""))))))
                 (range start end))))

(defn- symbol-flag-counts
  "Flags the cloud file raises by name for the chains of names among the
  tokens, matched by the whole chain or its last name."
  [compiled-flags name-chains]
  (if (empty? (:by-name compiled-flags))
    {}
    (reduce (fn [found {dotted :name}]
              (reduce (fn [counts flag-name] (update counts flag-name (fnil inc 0)))
                      found
                      (distinct (concat (get-in compiled-flags [:by-name dotted])
                                        (get-in compiled-flags [:by-name (last-segment dotted)])))))
            {}
            name-chains)))

(defn measure-span
  "Measurements of the tokens from `start` to `end`, shaped like
  complection/measure's, with the file's chains of names from
  `measuring`."
  [language-rules tokens start end nesting {:keys [compiled-flags text-lines] file-chains :name-chains} line end-line]
  (let [name-chains (chains-between file-chains start end)
        called (filter :called? name-chains)
        call-count (fn [names] (count (filter #(or (contains? names (:name %)) (contains? names (last-segment (:name %))))
                                              called)))]
    {:lines (inc (- end-line line))
     :branches (decision-count language-rules tokens start end)
     :nesting nesting
     :asynchronous (+ (word-count tokens start end (:asynchronous-words language-rules) (:asynchronous-operators language-rules))
                      (call-count (:asynchronous-calls language-rules)))
     :state (+ (word-count tokens start end (:state-words language-rules) #{})
               (call-count (:state-calls language-rules)))
     :flags (merge-with + (symbol-flag-counts compiled-flags name-chains)
                        (complection/text-flags compiled-flags (complection/span-text text-lines line end-line)))}))

(defn measured-form
  "A definition's entry among the measured forms, {:name :line :end-line
  :score :excesses}, or nil when it scores zero."
  [measurements thresholds {definition-name :name :keys [line end-line]}]
  (let [score (complection/score measurements thresholds)]
    (when (pos? score)
      {:name definition-name
       :line line
       :end-line end-line
       :score (complection/rounded score)
       :excesses (complection/excesses measurements thresholds)})))

(defn form-span
  "A top-level form from `line` to `end-line`, covering whole lines, as
  {:line :column :end-line :end-column :head :name}."
  [text-lines line end-line head form-name]
  {:line line
   :column 1
   :end-line end-line
   :end-column (inc (count (nth text-lines (dec end-line) "")))
   :head head
   :name form-name})

;; Statements

(defn head-index
  "Index of the token that names what a statement is, past async and the
  export, default, and declare that may open a script's statement."
  [tokens start end]
  (loop [index start]
    (if (and (< (inc index) end)
             (contains? #{"async" "export" "default" "declare" "abstract"} (:text (nth tokens index))))
      (recur (inc index))
      index)))

(defn body-start
  "Index of the first token after the colon that ends a compound
  statement's header, at the depth the statement starts at."
  [tokens depths {:keys [start end]}]
  (let [depth (nth depths start)]
    (or (some #(when (and (= ":" (:text (nth tokens %))) (= depth (nth depths %))) (inc %))
              (range start end))
        end)))

;; Braced bodies, as scripts and style sheets have

(def ^:private continuing-operators
  "Operators that carry a statement onto the next line when they end a
  line or begin the next."
  #{"=" "," "." "?." "=>" "+" "-" "*" "/" "%" "**" "&&" "||" "??" "?" ":" "|" "&" "<" ">" "==" "===" "!="
    "!==" "<=" ">=" "+=" "-=" "*=" "/=" "&&=" "||=" "??="})

(defn- item-break?
  "True when the token at `index`, at depth zero, starts a new top-level
  statement of a script or style sheet."
  [tokens depths index]
  (let [token (nth tokens index)
        previous (nth tokens (dec index))
        new-line? (> (:line token) (:line previous))]
    (and (zero? (nth depths index))
         (not (:continues? token))
         (or (and (= ";" (:text previous)) (zero? (nth depths (dec index))))
             (and new-line?
                  (not (contains? continuing-operators (:text previous)))
                  (not (opening? previous))
                  (not (contains? continuing-operators (:text token)))
                  (not (closing? token))
                  (not (contains? #{"else" "catch" "finally" "while"} (:text token))))))))

(defn items
  "[start end] for each top-level statement of a brace language's tokens."
  [tokens depths]
  (if (empty? tokens)
    []
    (loop [index 1
           start 0
           found []]
      (cond
        (>= index (count tokens)) (conj found [start (count tokens)])
        (item-break? tokens depths index) (recur (inc index) index (conj found [start index]))
        :else (recur (inc index) start found)))))

(defn first-index [tokens start end found?]
  (some #(when (found? (nth tokens %)) %) (range start (min end (count tokens)))))

(defn brace-nesting
  "Deepest nesting inside the brace at `open-index`, counting parentheses
  and braces as Clojure counts lists."
  [tokens depths open-index close-index]
  (let [base (nth depths open-index)]
    (reduce max 0 (keep #(let [{:keys [kind text]} (nth tokens %)]
                           (when (and (= :bracket kind) (contains? #{"(" "{"} text))
                             (- (nth depths %) base)))
                        (range (inc open-index) close-index)))))

;; Reading a file

(def no-structure
  "The structure of a file whose language defines nothing the engines
  read, such as an HTML page."
  {:definitions [] :top-level-forms [] :measured []})

(defn- imports-with-uses
  "`imports` with each bind's :uses, mapping the member reached, nil for
  the bare name, to its uses outside imports among the file's
  `name-chains`."
  [tokens name-chains imports]
  (let [import-lines (into #{} (mapcat (fn [{:keys [line end-line]}]
                                         (when line (range line (inc (or end-line line))))))
                           imports)
        chain-counts (frequencies (keep #(when-not (contains? import-lines (:line (nth tokens (:index %)))) (:name %))
                                        name-chains))
        uses-of (fn [local]
                  (let [prefix (str local ".")]
                    (reduce-kv (fn [tally chain-name use-count]
                                 (cond
                                   (= chain-name local) (update tally nil (fnil + 0) use-count)
                                   (string/starts-with? chain-name prefix)
                                   (update tally (first (string/split (subs chain-name (count prefix)) #"\."))
                                           (fnil + 0) use-count)
                                   :else tally))
                               {}
                               chain-counts)))]
    (mapv (fn [file-import]
            (update file-import :binds (fn [binds] (mapv #(assoc % :uses (uses-of (:local %))) binds))))
          imports)))

(defn- assigned?
  "True when the chain of names at `index` is the target of an
  assignment, which sets the name without using it."
  [tokens {:keys [index]}]
  (let [after (loop [position (inc index)]
                (if (and (= "." (:text (get tokens position))) (name-like? (get tokens (inc position))))
                  (recur (+ 2 position))
                  position))]
    (= "=" (:text (get tokens after)))))

(defn- local-uses
  "How often the file uses each of its own top-level definitions by name
  among its `name-chains`, outside the definitions' own names and the
  assignments that set them."
  [tokens name-chains definitions]
  (let [own-names (into #{} (comp (map :name) (remove #(string/includes? % "."))) definitions)]
    (frequencies (keep (fn [{chain-name :name index :index :as chain}]
                         (let [first-name (first (string/split chain-name #"\."))]
                           (when (and (contains? own-names first-name)
                                      (not= :definition (:kind (nth tokens index)))
                                      (not (assigned? tokens chain)))
                             first-name)))
                       name-chains))))

(defn read-structured
  "Facts of a scanned file, its plain facts with the imports
  `find-imports` finds and the structure `find-structure` finds."
  [language-id path text {:keys [flags complection]} find-imports find-structure]
  (let [normalized-text (source/normalize-text text)
        text-lines (source/split-lines normalized-text)
        all-tokens (scanner/token-stream language-id text-lines)
        merged (merged-operators (scanner/significant all-tokens))
        name-chains (chains merged 0 (count merged))
        compiled-flags (complection/compile-flags flags)
        thresholds (complection/thresholds-with complection)
        {:keys [definitions top-level-forms measured] :as structure} (find-structure merged text-lines
                                                                                     {:compiled-flags compiled-flags
                                                                                      :text-lines text-lines
                                                                                      :name-chains name-chains})
        imports (find-imports all-tokens)]
    (-> (plain/base-facts language-id path normalized-text text-lines {:flags flags})
        (merge (:facts structure))
        (assoc :definitions (vec definitions)
               :top-level-forms (vec top-level-forms)
               :measured-forms (->> measured
                                    (keep (fn [[entry measurements]] (measured-form measurements thresholds entry)))
                                    (sort-by :line)
                                    vec)
               :imports (if (some (comp seq :binds) imports)
                          (imports-with-uses merged name-chains imports)
                          imports)
               :local-references (local-uses merged name-chains definitions))
        (update :flag-counts #(merge-with + % (symbol-flag-counts compiled-flags name-chains))))))
