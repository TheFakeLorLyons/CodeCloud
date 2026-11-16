(ns codecloud.languages.python
  "Reads Python files for their structure, found by indentation, and for
  their imports, as dotted names with any leading relative dots, and the
  names each binds."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure
             :refer [body-start bracket-depths first-index form-span head-index measure-span name-like? rules]]))

;; Structure, by indentation

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

(defn- compound? [tokens {:keys [end]}]
  (= ":" (:text (nth tokens (dec end)))))

(defn- deepest-call [tokens depths level start end]
  (reduce max 0 (keep #(when (= "(" (:text (nth tokens %))) (+ level 1 (nth depths %)))
                      (range start end))))

(defn- python-nesting
  "Deepest nesting of a function body, statements counting one level below
  their compound statement and calls one below what encloses them."
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

(defn- module-statement-indexes [statements]
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
  "Top-level forms of a Python file, each a statement at the module's
  level with the block it opens, and decorators joined to what they
  decorate."
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

(defn- python-structure
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


;; Imports

(defn- statement-start? [tokens index]
  (let [previous (scanner/token-at tokens (dec index))
        current (scanner/token-at tokens index)]
    (or (nil? previous)
        (contains? #{";" ":"} (:text previous))
        (and (> (:line current) (:line previous)) (not= "\\" (:text previous))))))

(defn- statement-tokens
  "The tokens of the logical line starting at `start`, which runs on
  while brackets stay open or a line ends with a backslash."
  [tokens start]
  (loop [index start
         depth 0
         collected []]
    (let [{:keys [kind text line] :as token} (scanner/token-at tokens index)
          previous (peek collected)]
      (if (or (nil? token)
              (and previous
                   (zero? depth)
                   (or (= ";" text)
                       (and (> line (:line previous)) (not= "\\" (:text previous))))))
        collected
        (recur (inc index)
               (cond
                 (and (= :bracket kind) (contains? #{"(" "[" "{"} text)) (inc depth)
                 (and (= :bracket kind) (contains? #{")" "]" "}"} text)) (max 0 (dec depth))
                 :else depth)
               (conj collected token))))))

(defn- name-token?
  "True for a token that can be part of a module name, which reserved
  words such as import never are."
  [{:keys [kind]}]
  (contains? #{:text :definition} kind))

(defn- dotted-name
  "[name rest] for the dotted name at the start of `tokens`, leading dots
  kept, or [\"\" tokens]."
  [tokens]
  (loop [remaining tokens
         parts []]
    (let [{:keys [text] :as token} (first remaining)]
      (cond
        (and token (= "." text)) (recur (rest remaining) (conj parts "."))
        (and token (name-token? token) (re-matches #"[A-Za-z_][A-Za-z0-9_]*" text)
             (or (empty? parts) (= "." (peek parts))))
        (recur (rest remaining) (conj parts text))
        :else [(apply str parts) remaining]))))

(defn- aliased-names
  "[{:name :local}] for a comma-separated list of names, each with an
  optional `as` alias, read with `read-name`."
  [tokens read-name]
  (loop [remaining (remove #(contains? #{"(" ")" "\\"} (:text %)) tokens)
         found []]
    (if (empty? remaining)
      found
      (let [[imported after-name] (read-name remaining)
            aliased? (= "as" (:text (first after-name)))
            local (if aliased? (:text (second after-name)) imported)
            after-alias (if aliased? (drop 2 after-name) after-name)
            after-comma (if (= "," (:text (first after-alias))) (rest after-alias) after-alias)]
        (if (or (string/blank? imported) (= after-comma remaining))
          found
          (recur after-comma (conj found {:name imported :local local})))))))

(defn- star-or-name [tokens]
  (if (= "*" (:text (first tokens)))
    ["*" (rest tokens)]
    (dotted-name tokens)))

(defn- import-statement
  "The imports one statement makes, from `import a.b as c, d` or `from
  .a import b as c`."
  [[head & body :as statement] line]
  (case (:text head)
    "import" (for [{imported :name local :local} (aliased-names body dotted-name)]
               {:specifier imported
                :kind :import
                :resolver :python
                :line line
                :end-line (:line (last statement))
                :binds [{:local local :name nil}]})
    "from" (let [[module after-module] (dotted-name body)]
             (when (and (seq module) (= "import" (:text (first after-module))))
               (let [names (aliased-names (rest after-module) star-or-name)]
                 [{:specifier module
                   :kind :import
                   :resolver :python
                   :line line
                   :end-line (:line (last statement))
                   :names (mapv :name names)
                   :binds (mapv (fn [{imported :name local :local}] {:local local :name imported}) names)}])))
    nil))

(defn imports
  "Every import the significant tokens of a Python file make, nested
  imports inside functions included."
  [tokens]
  (into []
        (comp (keep-indexed (fn [index {:keys [kind text]}]
                              (when (and (= :special kind)
                                         (contains? #{"import" "from"} text)
                                         (statement-start? tokens index))
                                index)))
              (mapcat (fn [index]
                        (import-statement (statement-tokens tokens index) (:line (nth tokens index))))))
        tokens))

(defn read-python
  "Facts of a Python file, which are its imports, definitions, top-level
  forms, and complection."
  [path text options]
  (structure/read-structured :python path text options
                             (comp imports scanner/significant)
                             python-structure))
