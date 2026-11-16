(ns codecloud.languages.javascript
  "Reads JavaScript and TypeScript files for their structure, found by
  their braces, and for their imports, re-exports, requires, and dynamic
  imports, each with the names it binds."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure
             :refer [body-start brace-nesting bracket-depths first-index form-span head-index items matching-close
                     measure-span name-like? opening? rules]]))

;; Structure, by braces

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
  "Definitions in one top-level script statement, which are functions,
  classes with their methods, constants, and TypeScript types."
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

(defn- script-structure
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


;; Imports

(defn- specifier-token?
  "True for a quoted string token, which names a module, as opposed to a
  template literal or a regular expression."
  [token]
  (and (= :string (:kind token)) (re-find #"^['\"]" (:text token))))

(defn- name-token? [token]
  (and token (not (contains? #{:bracket :string :number} (:kind token)))
       (re-matches #"[A-Za-z_$][A-Za-z0-9_$]*" (:text token))))

(defn- from-clause
  "Index of the module string after `from` in the statement starting at
  `start`, or nil."
  [tokens start]
  (let [longest-clause 400]
    (loop [index start]
      (let [token (scanner/token-at tokens index)]
        (cond
          (or (nil? token) (> (- index start) longest-clause) (= ";" (:text token))) nil
          (and (= "from" (:text token)) (specifier-token? (scanner/token-at tokens (inc index)))) (inc index)
          (and (> index start) (= :special (:kind token)) (contains? #{"import" "export"} (:text token))) nil
          :else (recur (inc index)))))))

(defn- braced-bindings
  "Bindings of `{a, b as c, type d}`, whose tokens start just inside the
  brace, as {:local :name}."
  [tokens]
  (loop [remaining tokens
         found []]
    (let [[first-token second-token third-token] remaining]
      (cond
        (or (nil? first-token) (= "}" (:text first-token))) found
        (= "," (:text first-token)) (recur (rest remaining) found)
        (and (= "type" (:text first-token)) (name-token? second-token) (not= "as" (:text second-token)))
        (recur (rest remaining) found)
        (and (name-token? first-token) (= "as" (:text second-token)) (name-token? third-token))
        (recur (drop 3 remaining) (conj found {:local (:text third-token) :name (:text first-token)}))
        (name-token? first-token) (recur (rest remaining) (conj found {:local (:text first-token) :name (:text first-token)}))
        :else (recur (rest remaining) found)))))

(defn- import-bindings
  "The names an import clause binds, from the tokens between import and
  from, which may be a default name, * as a namespace, and braced names."
  [clause]
  (let [clause (if (= "type" (:text (first clause))) (rest clause) clause)]
    (loop [remaining clause
           found []]
      (let [[first-token second-token third-token] remaining]
        (cond
          (nil? first-token) found
          (= "," (:text first-token)) (recur (rest remaining) found)
          (and (= "*" (:text first-token)) (= "as" (:text second-token)) (name-token? third-token))
          (recur (drop 3 remaining) (conj found {:local (:text third-token) :name nil}))
          (= "{" (:text first-token))
          (into found (braced-bindings (rest remaining)))
          (name-token? first-token) (recur (rest remaining) (conj found {:local (:text first-token) :name "default"}))
          :else found)))))

(defn- required-bindings
  "Names a require call binds, the module for `const x = require(...)` and
  its members for `const {a, b: c} = require(...)`."
  [tokens require-index]
  (let [equals (scanner/token-at tokens (dec require-index))
        before (scanner/token-at tokens (- require-index 2))]
    (cond
      (not= "=" (:text equals)) []
      (name-token? before) [{:local (:text before) :name nil}]
      (= "}" (:text before))
      (let [opening (some #(when (= "{" (:text (nth tokens %))) %) (range (- require-index 3) -1 -1))]
        (if opening
          (loop [remaining (subvec tokens (inc opening) (- require-index 2))
                 found []]
            (let [[first-token second-token third-token] remaining]
              (cond
                (nil? first-token) found
                (and (name-token? first-token) (= ":" (:text second-token)) (name-token? third-token))
                (recur (drop 3 remaining) (conj found {:local (:text third-token) :name (:text first-token)}))
                (name-token? first-token)
                (recur (rest remaining) (conj found {:local (:text first-token) :name (:text first-token)}))
                :else (recur (rest remaining) found))))
          []))
      :else [])))

(defn- import-at [tokens index]
  (let [{:keys [kind text line]} (nth tokens index)
        following (scanner/token-at tokens (inc index))
        module (fn [specifier-index binds]
                 {:specifier (scanner/unquoted (:text (nth tokens specifier-index)))
                  :kind :import
                  :resolver :javascript
                  :line line
                  :end-line (:line (nth tokens specifier-index))
                  :binds binds})]
    (cond
      (and (= :special kind) (= "import" text))
      (cond
        (specifier-token? following) (module (inc index) [])
        (and (= "(" (:text following)) (specifier-token? (scanner/token-at tokens (+ 2 index))))
        (module (+ 2 index) [])
        :else (when-let [specifier-index (from-clause tokens (inc index))]
                (module specifier-index (import-bindings (subvec tokens (inc index) (dec specifier-index))))))

      (and (= :special kind) (= "export" text)
           (contains? #{"{" "*" "type"} (:text following)))
      (when-let [specifier-index (from-clause tokens (inc index))]
        (module specifier-index []))

      (and (= "require" text)
           (not= "." (:text (scanner/token-at tokens (dec index))))
           (= "(" (:text following))
           (specifier-token? (scanner/token-at tokens (+ 2 index))))
      (module (+ 2 index) (required-bindings tokens index))

      :else nil)))

(defn imports
  "Every import, re-export, require, and dynamic import among the
  significant tokens of a script."
  [tokens]
  (into [] (keep #(import-at tokens %)) (range (count tokens))))

(defn read-script
  "Facts of a JavaScript or TypeScript file in `language-id`, which are
  its imports, definitions, top-level forms, and complection."
  [language-id path text options]
  (structure/read-structured language-id path text options
                             (comp imports scanner/significant)
                             script-structure))
