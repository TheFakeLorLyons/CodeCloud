(ns codecloud.languages.structure
  "Definitions, top-level spans, and complection for scanned languages.
  Indentation finds Python's blocks, braces those of JavaScript,
  TypeScript, and CSS, and keywords those of shell. C and C++ take their
  definitions from a syntax tree and their measurements from here.
  Nesting counts where Clojure would open a list, so scores compare
  across languages."
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

(defn- opening? [{:keys [kind text]}]
  (and (= :bracket kind) (contains? #{"(" "[" "{"} text)))

(defn- closing? [{:keys [kind text]}]
  (and (= :bracket kind) (contains? #{")" "]" "}"} text)))

(defn bracket-depths
  "How many brackets are open before each token, where a closing bracket
  stands at the depth of the bracket it closes."
  [tokens]
  (first (reduce (fn [[depths depth] token]
                   (let [here (if (closing? token) (max 0 (dec depth)) depth)]
                     [(conj depths here) (if (opening? token) (inc here) here)]))
                 [[] 0]
                 tokens)))

(defn- matching-close
  "Index of the bracket that closes the one opened at `open-index`, or the
  last index when the file ends first."
  [tokens depths open-index]
  (let [depth (nth depths open-index)]
    (or (some #(when (and (closing? (nth tokens %)) (= depth (nth depths %))) %)
              (range (inc open-index) (count tokens)))
        (dec (count tokens)))))

(defn- name-like? [token]
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
  "Per language, what adds complection: :decisions, :decision-operators,
  and :decision-directives, :asynchronous words and calls, and :state
  changes."
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

(defn- last-segment
  "The last name of a dotted chain: sleep of time.sleep."
  [dotted]
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

;; Indentation: Python

(defn- python-statements
  "Logical lines of significant Python tokens as {:start :end :line
  :end-line :indent}, continued by brackets, backslashes, and strings."
  [tokens depths]
  (let [statement (fn [start end]
                    {:start start
                     :end end
                     :line (:line (nth tokens start))
                     :end-line (:line (nth tokens (dec end)))
                     :indent (:column (nth tokens start))})]
    (if (empty? tokens)
      []
      (loop [index 1
             start 0
             found []]
        (if (>= index (count tokens))
          (conj found (statement start (count tokens)))
          (let [token (nth tokens index)
                previous (nth tokens (dec index))
                breaks? (and (zero? (nth depths index))
                             (not (:continues? token))
                             (or (= ";" (:text previous))
                                 (and (> (:line token) (:line previous)) (not= "\\" (:text previous)))))]
            (if breaks?
              (recur (inc index) index (conj found (statement start index)))
              (recur (inc index) start found))))))))

(defn- block-end
  "Index of the last statement in the block that statement `index` opens,
  the run of statements indented deeper than it, or `index` itself."
  [statements index]
  (let [indent (:indent (nth statements index))]
    (or (last (take-while #(> (:indent (nth statements %)) indent) (range (inc index) (count statements))))
        index)))

(defn- head-index
  "Index of the token that names what a statement is, past async and the
  export, default, and declare that may open a script's statement."
  [tokens start end]
  (loop [index start]
    (if (and (< (inc index) end)
             (contains? #{"async" "export" "default" "declare" "abstract"} (:text (nth tokens index))))
      (recur (inc index))
      index)))

(defn- body-start
  "Index of the first token after the colon that ends a compound
  statement's header, at the depth the statement starts at."
  [tokens depths {:keys [start end]}]
  (let [depth (nth depths start)]
    (or (some #(when (and (= ":" (:text (nth tokens %))) (= depth (nth depths %))) (inc %))
              (range start end))
        end)))

(defn- compound? [tokens {:keys [end]}]
  (= ":" (:text (nth tokens (dec end)))))

(defn- deepest-call
  "The deepest level a call's parenthesis reaches among the tokens from
  `start` to `end` of a statement at `level`."
  [tokens depths level start end]
  (reduce max 0 (keep #(when (= "(" (:text (nth tokens %))) (+ level 1 (nth depths %)))
                      (range start end))))

(defn- python-nesting
  "Deepest nesting of a function body: statements one level below their
  compound statement, calls one below what encloses them."
  [tokens depths statements first-index last-index inline-start inline-end]
  (loop [index first-index
         open-indents []
         deepest (deepest-call tokens depths 0 inline-start inline-end)]
    (if (> index last-index)
      deepest
      (let [{:keys [indent start end] :as statement} (nth statements index)
            enclosing (vec (take-while #(< % indent) open-indents))
            level (inc (count enclosing))]
        (recur (inc index)
               (conj enclosing indent)
               (max deepest
                    (if (compound? tokens statement) level 0)
                    (deepest-call tokens depths level start end)))))))

(defn- decorator? [tokens {:keys [start]}]
  (let [{:keys [kind text]} (nth tokens start)]
    (and (= :reader kind) (string/starts-with? text "@"))))

(defn- decorated-line
  "The line a definition starts on, the first of the decorators stacked
  right above it at its own indent."
  [tokens statements index]
  (let [{:keys [indent line]} (nth statements index)
        decorators (take-while #(and (decorator? tokens (nth statements %))
                                     (= indent (:indent (nth statements %))))
                               (range (dec index) -1 -1))]
    (if (seq decorators) (:line (nth statements (last decorators))) line)))

(defn- python-definition-entries
  "A Python file's definitions as {:name :kind :private? :line :end-line
  :measure}. Methods take their class's name; nested functions belong to
  the function holding them."
  [tokens depths statements]
  (loop [index 0
         enclosing []
         found []]
    (if (>= index (count statements))
      found
      (let [enclosing (filterv #(<= index (:last %)) enclosing)
            {:keys [start end] :as statement} (nth statements index)
            head (nth tokens (head-index tokens start end))
            name-token (get tokens (inc (head-index tokens start end)))]
        (if (and (= :special (:kind head)) (contains? #{"def" "class"} (:text head)) (name-like? name-token))
          (let [last-index (block-end statements index)
                kind (if (= "class" (:text head)) :type :function)
                inside-function? (some #(= :function (:kind %)) enclosing)
                inline-start (body-start tokens depths statement)
                entry {:name (string/join "." (concat (map :name enclosing) [(:text name-token)]))
                       :kind kind
                       :private? (string/starts-with? (:text name-token) "_")
                       :line (decorated-line tokens statements index)
                       :end-line (:end-line (nth statements last-index))
                       :measure (when (= :function kind)
                                  {:start inline-start
                                   :end (:end (nth statements last-index))
                                   :nesting (python-nesting tokens depths statements (inc index) last-index
                                                            inline-start end)})}]
            (recur (inc index)
                   (conj enclosing {:name (:text name-token) :kind kind :last last-index})
                   (if inside-function? found (conj found entry))))
          (recur (inc index) enclosing found))))))

(defn- module-statement-indexes
  "Indexes of the statements at a module's own level, outside every
  block."
  [statements]
  (loop [index 0
         found []]
    (if (>= index (count statements))
      found
      (recur (inc (block-end statements index)) (conj found index)))))

(defn- python-values
  "Module-level assignments such as RATE = 250 as value definitions."
  [tokens statements]
  (into []
        (keep (fn [index]
                (let [{:keys [start end line end-line]} (nth statements index)
                      name-token (nth tokens start)
                      next-token (get tokens (inc start))]
                  (when (and (< (inc start) end)
                             (name-like? name-token)
                             (not= :special (:kind name-token))
                             (contains? #{"=" ":"} (:text next-token)))
                    {:name (:text name-token)
                     :kind :value
                     :private? (string/starts-with? (:text name-token) "_")
                     :line line
                     :end-line end-line}))))
        (module-statement-indexes statements)))

(defn- python-forms
  "Top-level forms of a Python file: each statement at the module's level
  with the block it opens, decorators joined to what they decorate."
  [tokens statements text-lines]
  (loop [indexes (module-statement-indexes statements)
         pending-line nil
         found []]
    (if-let [index (first indexes)]
      (let [{:keys [start end line]} (nth statements index)
            head-at (head-index tokens start end)
            head (nth tokens head-at)
            named (get tokens (inc head-at))
            last-statement (nth statements (block-end statements index))]
        (if (decorator? tokens (nth statements index))
          (recur (rest indexes) (or pending-line line) found)
          (recur (rest indexes)
                 nil
                 (conj found (form-span text-lines (or pending-line line) (:end-line last-statement)
                                        (:text head)
                                        (when (and (contains? #{"def" "class"} (:text head)) (name-like? named))
                                          (:text named)))))))
      found)))

(defn python-structure
  "{:definitions :top-level-forms :measured} of a Python file's
  significant tokens, operators merged."
  [tokens text-lines measuring]
  (let [depths (bracket-depths tokens)
        statements (python-statements tokens depths)
        entries (python-definition-entries tokens depths statements)]
    {:definitions (into (mapv #(dissoc % :measure) entries) (python-values tokens statements))
     :top-level-forms (python-forms tokens statements text-lines)
     :measured (for [{:keys [measure line end-line] :as entry} entries
                     :when measure]
                 [entry (measure-span (:python rules) tokens (:start measure) (:end measure) (:nesting measure)
                                      measuring line end-line)])}))

;; Braces: JavaScript, TypeScript, and style sheets

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

(defn- items
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

(defn- first-index
  "Index of the first token from `start` to `end` that passes `found?`."
  [tokens start end found?]
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

(defn- function-body
  "[open close] of a function body's braces after `params-index`, or [nil
  arrow-index] for an expression-bodied arrow."
  [tokens depths params-index end]
  (let [params-close (matching-close tokens depths params-index)
        depth (nth depths params-index)
        brace (first-index tokens (inc params-close) end #(and (= "{" (:text %)) (= :bracket (:kind %))))
        arrow (first-index tokens (inc params-close) end #(= "=>" (:text %)))]
    (cond
      (and arrow (or (nil? brace) (< arrow brace)) (not= "{" (:text (get tokens (inc arrow)))))
      [nil (inc arrow)]
      (and brace (= depth (nth depths brace))) [brace (matching-close tokens depths brace)]
      :else nil)))

(defn- script-function
  "A measured function entry named `function-name` whose parameters open
  at `params-index`, spanning from `line` to its body's end, or nil."
  [tokens depths params-index end function-name line]
  (when-let [[open close] (function-body tokens depths params-index end)]
    (let [[body-start body-end] (if open [(inc open) close] [close end])
          last-token (nth tokens (dec (if open (inc close) end)))]
      {:name function-name
       :kind :function
       :private? (string/starts-with? function-name "_")
       :line line
       :end-line (:line last-token)
       :measure {:start body-start
                 :end body-end
                 :nesting (if open
                            (brace-nesting tokens depths open close)
                            (let [base (nth depths body-start)]
                              (inc (reduce max 0 (keep #(when (opening? (nth tokens %)) (- (nth depths %) base))
                                                       (range body-start body-end))))))}})))

(def ^:private member-modifiers
  #{"static" "async" "get" "set" "public" "private" "protected" "readonly" "override" "abstract" "*"})

(defn- class-methods
  "Measured entries for the methods in the class body between the braces
  at `open` and `close`, named after the class."
  [tokens depths open close class-name]
  (let [member-depth (inc (nth depths open))]
    (into []
          (keep (fn [index]
                  (let [token (nth tokens index)
                        name-token (get tokens (dec index))
                        before-name (get tokens (- index 2))]
                    (when (and (= "(" (:text token))
                               (= member-depth (nth depths index))
                               (name-like? name-token)
                               (or (= open (- index 2))
                                   (contains? #{"}" ";"} (:text before-name))
                                   (contains? member-modifiers (:text before-name))))
                      (script-function tokens depths index close (str class-name "." (:text name-token))
                                       (:line name-token))))))
          (range (inc open) close))))

(defn- script-item-entries
  "Definitions in one top-level script statement: functions, classes with
  their methods, constants, and TypeScript types."
  [tokens depths [start end]]
  (let [head-at (head-index tokens start end)
        head (nth tokens head-at)
        line (:line (nth tokens start))]
    (case (:text head)
      "function" (let [name-at (if (= "*" (:text (get tokens (inc head-at)))) (+ 2 head-at) (inc head-at))
                       name-token (get tokens name-at)
                       params (first-index tokens name-at end #(= "(" (:text %)))]
                   (when (and (name-like? name-token) params)
                     (some-> (script-function tokens depths params end (:text name-token) line) vector)))
      "class" (let [name-token (get tokens (inc head-at))
                    open (first-index tokens (inc head-at) end #(and (= "{" (:text %)) (= :bracket (:kind %))))]
                (when (name-like? name-token)
                  (let [close (when open (matching-close tokens depths open))
                        last-line (:line (nth tokens (dec end)))]
                    (into [{:name (:text name-token) :kind :type :private? false :line line :end-line last-line}]
                          (when open (class-methods tokens depths open close (:text name-token)))))))
      ("const" "let" "var") (let [name-token (get tokens (inc head-at))
                                  equals (first-index tokens (inc head-at) end #(= "=" (:text %)))
                                  value (when equals (head-index tokens (inc equals) end))
                                  value-token (when value (get tokens value))
                                  params (cond
                                           (nil? value-token) nil
                                           (= "function" (:text value-token)) (first-index tokens value end #(= "(" (:text %)))
                                           (= "(" (:text value-token)) value
                                           (and (name-like? value-token) (= "=>" (:text (get tokens (inc value))))) value
                                           :else nil)]
                              (when (name-like? name-token)
                                (if-let [function-entry (when params
                                                          (script-function tokens depths params end (:text name-token) line))]
                                  [function-entry]
                                  [{:name (:text name-token)
                                    :kind :value
                                    :private? (string/starts-with? (:text name-token) "_")
                                    :line line
                                    :end-line (:line (nth tokens (dec end)))}])))
      ("interface" "type" "enum" "namespace") (let [name-token (get tokens (inc head-at))]
                                                (when (name-like? name-token)
                                                  [{:name (:text name-token) :kind :type :private? false :line line
                                                    :end-line (:line (nth tokens (dec end)))}]))
      nil)))

(defn script-structure
  "{:definitions :top-level-forms :measured} of a JavaScript or TypeScript
  file's significant tokens, operators merged."
  [tokens text-lines measuring]
  (let [depths (bracket-depths tokens)
        statement-ranges (items tokens depths)
        entries (into [] (mapcat #(script-item-entries tokens depths %)) statement-ranges)]
    {:definitions (mapv #(dissoc % :measure) entries)
     :top-level-forms (mapv (fn [[start end]]
                              (let [head (nth tokens (head-index tokens start end))]
                                (form-span text-lines (:line (nth tokens start)) (:line (nth tokens (dec end)))
                                           (:text head)
                                           (some #(when (= (:line %) (:line (nth tokens start))) (:name %)) entries))))
                            statement-ranges)
     :measured (for [{:keys [measure line end-line] :as entry} entries
                     :when measure]
                 [entry (measure-span (:javascript rules) tokens (:start measure) (:end measure) (:nesting measure)
                                      measuring line end-line)])}))

(defn style-sheet-structure
  "{:definitions :top-level-forms :measured} of a style sheet's
  significant tokens, operators merged: each rule and at-rule is a form,
  and Sass mixins and functions are definitions."
  [tokens text-lines measuring]
  (let [depths (bracket-depths tokens)
        statement-ranges (items tokens depths)
        entries (into []
                      (keep (fn [[start end]]
                              (let [head (nth tokens start)
                                    name-token (get tokens (inc start))]
                                (cond
                                  (and (contains? #{"@mixin" "@function"} (:text head)) name-token)
                                  (let [open (first-index tokens start end #(and (= "{" (:text %)) (= :bracket (:kind %))))
                                        close (when open (matching-close tokens depths open))]
                                    {:name (:text name-token)
                                     :kind :function
                                     :private? false
                                     :line (:line head)
                                     :end-line (:line (nth tokens (dec end)))
                                     :measure (when open
                                                {:start (inc open)
                                                 :end close
                                                 :nesting (brace-nesting tokens depths open close)})})

                                  (and (= :reader (:kind head)) (string/starts-with? (:text head) "$")
                                       (= ":" (:text name-token)))
                                  {:name (subs (:text head) 1)
                                   :kind :value
                                   :private? false
                                   :line (:line head)
                                   :end-line (:line (nth tokens (dec end)))}

                                  :else nil))))
                      statement-ranges)]
    {:definitions (mapv #(dissoc % :measure) entries)
     :top-level-forms (mapv (fn [[start end]]
                              (form-span text-lines (:line (nth tokens start)) (:line (nth tokens (dec end)))
                                         (:text (nth tokens start)) nil))
                            statement-ranges)
     :measured (for [{:keys [measure line end-line] :as entry} entries
                     :when measure]
                 [entry (measure-span (:css rules) tokens (:start measure) (:end measure) (:nesting measure)
                                      measuring line end-line)])}))

;; Keywords: shell scripts

(def ^:private shell-openers #{"if" "for" "while" "until" "case" "select"})
(def ^:private shell-closers #{"fi" "done" "esac"})

(defn- shell-opener? [{:keys [kind text]}]
  (or (and (= :special kind) (contains? shell-openers text))
      (and (= :bracket kind) (contains? #{"(" "{"} text))
      (and (= :reader kind) (= "$(" text))))

(defn- shell-closer? [{:keys [kind text]}]
  (or (and (= :special kind) (contains? shell-closers text))
      (and (= :bracket kind) (contains? #{")" "}"} text))
      (and (= :reader kind) (= ")" text))))

(defn- shell-depths
  "How many compound commands, groups, and substitutions are open before
  each token, a closer standing at the depth of what it closes."
  [tokens]
  (first (reduce (fn [[depths depth] token]
                   (let [here (if (shell-closer? token) (max 0 (dec depth)) depth)]
                     [(conj depths here) (if (shell-opener? token) (inc here) here)]))
                 [[] 0]
                 tokens)))

(defn- shell-items
  "[start end] of each top-level command, compound commands whole,
  following backslash, pipe, and and-or continuations."
  [tokens depths]
  (if (empty? tokens)
    []
    (loop [index 1
           start 0
           found []]
      (if (>= index (count tokens))
        (conj found [start (count tokens)])
        (let [token (nth tokens index)
              previous (nth tokens (dec index))
              breaks? (and (zero? (nth depths index))
                           (not (:continues? token))
                           (not (shell-closer? token))
                           (> (:line token) (:line previous))
                           (not (contains? #{"\\" "|" "&&" "||"} (:text previous)))
                           (not (and (= :reader (:kind previous)) (string/starts-with? (:text previous) "<<"))))]
          (if breaks?
            (recur (inc index) index (conj found [start index]))
            (recur (inc index) start found)))))))

(defn- shell-function
  "The function a top-level command defines, as `name() {` or `function
  name {`, with its body measured, or nil."
  [tokens depths [start end]]
  (let [function-word? (= "function" (:text (nth tokens start)))
        name-token (nth tokens (if function-word? (inc start) start))
        open (first-index tokens start end #(and (= "{" (:text %)) (= :bracket (:kind %))))]
    (when (and open
               (= :definition (:kind name-token))
               (<= (- open start) 4))
      (let [close (matching-close tokens depths open)
            base (nth depths open)]
        {:name (:text name-token)
         :kind :function
         :private? (string/starts-with? (:text name-token) "_")
         :line (:line (nth tokens start))
         :end-line (:line (nth tokens close))
         :measure {:start (inc open)
                   :end close
                   :nesting (reduce max 0 (keep #(when (shell-opener? (nth tokens %)) (- (nth depths %) base))
                                                (range (inc open) close)))}}))))

(defn shell-structure
  "{:definitions :top-level-forms :measured} of a shell script's
  significant tokens, operators merged."
  [tokens text-lines measuring]
  (let [depths (shell-depths tokens)
        statement-ranges (shell-items tokens depths)
        entries (into [] (keep #(shell-function tokens depths %)) statement-ranges)]
    {:definitions (mapv #(dissoc % :measure) entries)
     :top-level-forms (mapv (fn [[start end]]
                              (form-span text-lines (:line (nth tokens start)) (:line (nth tokens (dec end)))
                                         (:text (nth tokens start))
                                         (some #(when (= (:line %) (:line (nth tokens start))) (:name %)) entries)))
                            statement-ranges)
     :measured (for [{:keys [measure line end-line] :as entry} entries]
                 [entry (measure-span (:shell rules) tokens (:start measure) (:end measure) (:nesting measure)
                                      measuring line end-line)])}))

;; Reading a file

(def no-structure
  "The structure of a file whose language defines nothing the engines
  read, such as an HTML page."
  {:definitions [] :top-level-forms [] :measured []})

(defn- imports-with-uses
  "`imports` with each bind's :uses: the member reached (nil for the bare
  name) to its uses outside imports, among the file's `name-chains`."
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
    (mapv (fn [import]
            (update import :binds (fn [binds] (mapv #(assoc % :uses (uses-of (:local %))) binds))))
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
  "Facts of a scanned file: plain facts, imports from `find-imports` with
  their uses, and structure from `find-structure`, given the significant
  tokens with operators merged and measured with the cloud file's
  :complection and :flags. Structure may add :facts of its own."
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
