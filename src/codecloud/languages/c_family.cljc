(ns codecloud.languages.c-family
  "Reads C and C++: the headers each file includes, the names it uses,
  and its definitions, measured with the shared token rules. tree-sitter's
  syntax tree says what a file defines and where, and the braces among
  the tokens say where each body ends, which a macro or an #if can hide
  from the tree. A header joins its source file's page when the pairing
  is unique."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure]
            [codecloud.model.source :as source]))

;; Queries, in tree-sitter's query language. A capture whose name ends in
;; name carries its node's text.

(def ^:private shared-patterns
  "What both grammars answer: functions with their bodies, storage, and
  any macro call standing in for a signature, then types, macros, values
  at file scope, and the top-level forms."
  ["(function_definition body: (_) @function.body) @function"
   "(function_definition !type declarator: (function_declarator) @function.call.name) @function.untyped"
   "(function_definition (storage_class_specifier) @function.storage.name) @function.stored"
   "(struct_specifier name: (_) @type.name body: (_) @type.body) @type"
   "(union_specifier name: (_) @type.name body: (_) @type.body) @type"
   "(enum_specifier name: (_) @type.name body: (_)) @type"
   "(type_definition declarator: (type_identifier) @type.name) @type"
   "(preproc_function_def name: (identifier) @macro.name) @macro"
   "(preproc_def name: (identifier) @value.name value: (_)) @value"
   "(translation_unit (declaration declarator: (init_declarator declarator: (_) @value.name)) @value)"
   "(translation_unit (_) @form)"])

(def ^:private c-patterns
  ["(function_declarator declarator: (identifier) @function.name)"])

(def ^:private cpp-patterns
  "C++ names functions by member, scope, destructor, operator, and
  conversion too, and adds classes, aliases, concepts, unnamed namespaces,
  and the insides of namespaces and extern blocks."
  ["(function_declarator declarator: [(identifier) (field_identifier) (qualified_identifier) (destructor_name) (operator_name) (template_function)] @function.name)"
   "(function_definition declarator: [(operator_cast) (qualified_identifier name: (operator_cast))] @function.name)"
   "(class_specifier name: (_) @type.name body: (_) @type.body) @type"
   "(alias_declaration name: (type_identifier) @type.name) @type"
   "(concept_definition name: (identifier) @type.name) @type"
   "(namespace_definition !name body: (_) @unnamed.namespace)"
   "(declaration_list (declaration declarator: (init_declarator declarator: (_) @value.name)) @value)"
   "(declaration_list (_) @form)"])

(def queries
  "The query each grammar answers, by grammar id."
  {:c (string/join "\n" (concat shared-patterns c-patterns))
   :cpp (string/join "\n" (concat shared-patterns cpp-patterns))})

;; Choosing C or C++

(def ^:private cpp-words
  "Words C does not have, whose presence makes a header C++."
  #{"namespace" "template" "typename" "virtual" "constexpr" "nullptr" "noexcept" "decltype" "explicit"})

(defn cpp-code?
  "True when significant tokens, read as C++, hold code only C++ writes:
  a scope operator, a word of C++'s own, a class with a body or a base,
  or an access label."
  [tokens]
  (let [merged (structure/merged-operators tokens)]
    (boolean
     (some (fn [index]
             (let [{:keys [kind text]} (nth merged index)
                   following (:text (get merged (inc index)))
                   after-following (:text (get merged (+ 2 index)))]
               (or (= "::" text)
                   (and (contains? #{:special :constant} kind) (contains? cpp-words text))
                   (and (= "class" text) (contains? #{"{" ":" "final"} after-following))
                   (and (contains? #{"public" "private" "protected"} text) (= ":" following)))))
           (range (count merged))))))

(def ^:private cpp-marks
  "Text a header holds somewhere when its code may be C++, found before
  lexing settles the question."
  (re-pattern (str "::|\\b(?:"
                   (string/join "|" (sort (into cpp-words ["class" "public" "private" "protected"])))
                   ")\\b")))

(defn file-language
  "C++ for a file of a C++ extension, and for a .h file whose code is C++;
  C otherwise."
  [language-id path text-lines]
  (if (and (= :c language-id)
           (= "h" (source/file-extension path))
           (some #(re-find cpp-marks %) text-lines)
           (cpp-code? (scanner/significant (scanner/token-stream :cpp text-lines))))
    :cpp
    language-id))

;; Includes and the names a file uses

(defn includes
  "Every header the significant tokens include, quoted or in angle
  brackets, as an import for the include resolver."
  [tokens]
  (into []
        (keep (fn [index]
                (let [{:keys [kind text line]} (nth tokens index)
                      {header :text header-kind :kind} (scanner/token-at tokens (inc index))]
                  (when (and (= :reader kind)
                             (re-matches #"#\s*(?:include_next|include|import)" text)
                             (= :string header-kind)
                             (>= (count header) 2))
                    {:specifier (subs header 1 (dec (count header)))
                     :kind :include
                     :resolver :c-include
                     :system? (string/starts-with? header "<")
                     :line line
                     :binds []}))))
        (range (count tokens))))

(def ^:private name-starts
  "The characters a name can begin with."
  (set (map str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")))

(defn- identifier-counts
  "How often each name stands in the code, which resolution matches
  against the definitions of the headers a file includes."
  [tokens]
  (frequencies (keep (fn [{:keys [kind text]}]
                       (when (and (contains? #{:text :definition} kind) (contains? name-starts (subs text 0 1)))
                         text))
                     tokens)))

;; Positions, each one number so that comparing them is cheap

(defn- position
  "One number ordering [line column] positions as the text does. Lines
  past ten million characters never reach here, since files that long in
  a line are read as minified and left out."
  [line column]
  (let [columns-per-line 10000000]
    (+ (* line columns-per-line) column)))

(defn- start-of [{:keys [line column]}]
  (position line column))

(defn- last-line
  "The last line a node covers, leaving out a line it reaches only by
  taking the newline before it, as preprocessor lines do."
  [{:keys [line end-line end-column]}]
  (if (and (= 1 end-column) (> end-line line)) (dec end-line) end-line))

;; Braces among the tokens

(defn- brace-closes
  "Map from the index of each { among the tokens to the index of the }
  closing it."
  [tokens]
  (loop [index 0
         open-indexes ()
         closes (transient {})]
    (if (>= index (count tokens))
      (persistent! closes)
      (let [{:keys [kind text]} (nth tokens index)]
        (cond
          (not= :bracket kind) (recur (inc index) open-indexes closes)
          (= "{" text) (recur (inc index) (conj open-indexes index) closes)
          (and (= "}" text) (seq open-indexes)) (recur (inc index) (rest open-indexes)
                                                       (assoc! closes (first open-indexes) index))
          :else (recur (inc index) open-indexes closes))))))

(defn- token-layout
  "What finding definitions among tokens needs: the tokens, the position
  of each, their bracket depths, where each { closes, and the lines that
  hold preprocessor directives."
  [tokens]
  {:tokens tokens
   :positions (mapv start-of tokens)
   :depths (structure/bracket-depths tokens)
   :closes (brace-closes tokens)
   :directive-lines (into #{}
                          (keep #(when (and (= :reader (:kind %)) (string/starts-with? (:text %) "#")) (:line %)))
                          tokens)})

(defn- index-at
  "Index of the first token at or after `position`."
  [positions position]
  (loop [low 0
         high (count positions)]
    (if (< low high)
      (let [middle (quot (+ low high) 2)]
        (if (< (nth positions middle) position)
          (recur (inc middle) high)
          (recur low middle)))
      low)))

(defn- brace-block
  "[open close] token indexes of the braces a capture starts with, or nil
  when no { stands at its start."
  [{:keys [tokens positions closes]} capture]
  (let [start (start-of capture)
        index (index-at positions start)]
    (when (and (< index (count positions))
               (= start (nth positions index))
               (= "{" (:text (nth tokens index))))
      (when-let [close (get closes index)]
        [index close]))))

(defn- brace-extent
  "{:start :end}, the positions of the braces a capture starts with, or
  nil."
  [layout capture]
  (when-let [[open close] (brace-block layout capture)]
    {:start (nth (:positions layout) open)
     :end (nth (:positions layout) close)}))

(defn- enclosing
  "The innermost of `extents`, sorted by start, holding `position`."
  [extents position]
  (last (filter #(< (:start %) position (:end %)) extents)))

(defn- declaration-end?
  "True for a token after which a declaration can begin: a brace, an
  access label's colon, or anything ending in a semicolon, such as the
  >; closing a template argument list."
  [{:keys [text]}]
  (or (contains? #{"{" "}" ":"} text) (string/ends-with? text ";")))

(defn- declaration-line
  "The line a definition whose name stands at token `name-index` begins
  on: after the last ; { } or : before the name, past preprocessor lines,
  so a template line or a return type on its own line belongs to it."
  [{:keys [tokens directive-lines]} name-index]
  (let [after-end (loop [index (dec name-index)]
                    (cond
                      (neg? index) 0
                      (declaration-end? (nth tokens index)) (inc index)
                      :else (recur (dec index))))
        first-code (loop [index after-end]
                     (if (and (< index name-index) (contains? directive-lines (:line (nth tokens index))))
                       (recur (inc index))
                       index))]
    (:line (nth tokens first-code))))

;; Names

(defn- word-at?
  "True when `word` stands alone at `index` of `text`."
  [text index word]
  (let [word-character? (fn [character] (boolean (and character (re-matches #"[A-Za-z0-9_]" (str character)))))]
    (and (not (word-character? (get text (dec index))))
         (not (word-character? (get text (+ index (count word))))))))

(defn- operator-start
  "Index of the word operator in `text`, or nil."
  [text]
  (loop [from 0]
    (when-let [index (string/index-of text "operator" from)]
      (if (word-at? text index "operator") index (recur (inc index))))))

(defn- without-template-arguments
  "`text` without the template arguments of its names, as Ring<T> reads
  Ring, up to the word operator, so operator<< stays whole."
  [text]
  (let [cut (or (operator-start text) (count text))
        stripped (loop [remaining (subs text 0 cut)]
                   (let [shorter (string/replace remaining #"<[^<>]*>" "")]
                     (if (= shorter remaining) remaining (recur shorter))))]
    (str stripped (subs text cut))))

(defn- without-cast-parameters
  "A conversion operator's name without the parameters and qualifiers its
  node also holds, as operator bool."
  [text]
  (let [start (operator-start text)
        parenthesis (when start (string/index-of text "(" start))]
    (if (and parenthesis (re-find #"^operator\s+[A-Za-z_]" (subs text start)))
      (string/trimr (subs text 0 parenthesis))
      text)))

(defn- clean-name
  "A name as written, with one space between words, no pointer or
  reference marks before it, no template arguments, and no leading global
  scope."
  [text]
  (-> (string/replace text #"\s+" " ")
      string/trim
      (string/replace #"^[*&\s]+" "")
      without-cast-parameters
      without-template-arguments
      (string/replace #"^::" "")))

(defn- value-name
  "The name a value's declarator gives, without array sizes or the
  parentheses of a pointer to a function."
  [text]
  (or (re-find #"[A-Za-z_][A-Za-z0-9_]*(?:::[A-Za-z_][A-Za-z0-9_]*)*" (clean-name text))
      (clean-name text)))

(defn- macro-call-name?
  "True for a name in capitals, as macros such as TEST are written."
  [text]
  (boolean (re-matches #"[A-Z][A-Z0-9_]*" text)))

(defn- qualified
  "`own-name` led by `scope-name`, unless it already names its scope, as
  an out-of-class Board::read does."
  [scope-name own-name]
  (if (and scope-name (not (string/includes? own-name "::")))
    (str scope-name "::" own-name)
    own-name))

;; Definitions

(def ^:private match-kinds
  "The capture that tells what a match is, tried in this order."
  [:function :function.untyped :function.stored :function.name :type :macro :value :unnamed.namespace :form])

(defn- match-kind [match]
  (some #(when (contains? match %) %) match-kinds))

(defn- class-scopes
  "Each class, struct, or union body as {:name :start :end}, sorted by
  start and named after the bodies around it, as Ring::Node."
  [type-matches layout]
  (reduce (fn [scopes {:keys [own-name start] :as extent}]
            (conj scopes (-> extent
                             (dissoc :own-name)
                             (assoc :name (qualified (:name (enclosing scopes start)) own-name)))))
          []
          (sort-by :start
                   (keep (fn [{body :type.body type-name :type.name}]
                           (some-> (when body (brace-extent layout body))
                                   (assoc :own-name (clean-name (:text type-name)))))
                         type-matches))))

(defn- outermost
  "Those of `functions`, sorted by where their bodies start, whose bodies
  lie inside no other's. Only a macro puts one function inside another,
  as Lua's vmdispatch (op) { ... } inside luaV_execute does."
  [functions]
  (first (reduce (fn [[kept outer-end] {:keys [body-start body-end] :as function}]
                   (if (and outer-end (< body-start outer-end))
                     [kept outer-end]
                     [(conj kept function) body-end]))
                 [[] nil]
                 functions)))

(defn- function-entries
  "A definition for each function with a brace body, named by the last
  name before its body and after its class, with the token indexes of its
  braces as :body. A macro standing in for a signature, as with
  TEST(Suite, Case), names the function by its whole call."
  [by-kind layout scopes private-scopes]
  (let [{:keys [tokens positions]} layout
        names (into (sorted-map) (map (fn [{capture :function.name}] [(start-of capture) capture])) (:function.name by-kind))
        macro-calls (into {}
                          (map (fn [{node :function.untyped call :function.call.name}] [(start-of node) call]))
                          (:function.untyped by-kind))
        static-starts (into #{}
                            (keep (fn [{node :function.stored storage :function.storage.name}]
                                    (when (= "static" (:text storage)) (start-of node))))
                            (:function.stored by-kind))
        functions (sort-by :body-start
                           (keep (fn [{node :function body :function.body}]
                                   (when-let [[open close] (brace-block layout body)]
                                     {:node node
                                      :start (start-of node)
                                      :body [open close]
                                      :body-start (nth positions open)
                                      :body-end (nth positions close)}))
                                 (:function by-kind)))]
    (keep (fn [{:keys [start body body-start]}]
            (when-let [[name-start name-capture] (first (rsubseq names < body-start))]
              (when (>= name-start start)
                (let [own-name (clean-name (:text name-capture))
                      scope (enclosing scopes body-start)
                      macro-call (get macro-calls start)
                      macro-defined? (boolean (and macro-call (nil? scope) (macro-call-name? own-name)))]
                  {:name (if macro-defined?
                           (string/replace (:text macro-call) #"\s+" " ")
                           (qualified (:name scope) own-name))
                   :kind (if (and macro-defined? (string/includes? own-name "TEST")) :test :function)
                   :private? (boolean (or (enclosing private-scopes body-start)
                                          (and (nil? scope) (contains? static-starts start))))
                   :line (declaration-line layout (index-at positions name-start))
                   :end-line (:line (nth tokens (second body)))
                   :body body}))))
          (outermost functions))))

(defn- named-entries
  "A definition of `kind` for each of `matches`, whose node is captured as
  `kind` and its name as kind.name, read through `name-of`: led by its
  class among `scopes`, starting with any template line before it, and
  ending where its braces do when it has a body."
  [kind name-of matches layout scopes private-scopes]
  (map (fn [match]
         (let [node (get match kind)
               name-capture (get match (keyword (str (name kind) ".name")))
               start (start-of node)
               block (when-let [body (:type.body match)] (brace-block layout body))]
           {:name (qualified (:name (enclosing scopes start)) (name-of (:text name-capture)))
            :kind kind
            :private? (boolean (enclosing private-scopes start))
            :line (if (contains? (:directive-lines layout) (:line node))
                    (:line node)
                    (min (:line node) (declaration-line layout (index-at (:positions layout) (start-of name-capture)))))
            :end-line (if block (:line (nth (:tokens layout) (second block))) (last-line node))}))
       matches))

;; Structure

(def ^:private container-types
  "Nodes whose insides are forms of their own."
  #{"namespace_definition" "linkage_specification"})

(defn- top-level-forms
  "A form for each declaration, definition, and directive at file scope or
  directly inside a namespace or extern block. A #if block is one form."
  [form-matches {:keys [tokens positions]} text-lines entries]
  (let [name-on-line (reduce (fn [names {entry-name :name :keys [line]}]
                               (if (contains? names line) names (assoc names line entry-name)))
                             {}
                             entries)]
    (->> form-matches
         (map :form)
         (remove #(or (= "comment" (:type %)) (contains? container-types (:type %))))
         (sort-by start-of)
         (mapv (fn [{:keys [line] :as form}]
                 (let [index (index-at positions (start-of form))]
                   (structure/form-span text-lines line (last-line form)
                                        (if (< index (count tokens)) (:text (nth tokens index)) (:type form))
                                        (get name-on-line line))))))))

(defn- tree-structure
  "{:definitions :top-level-forms :measured :facts} of a file from the
  `matches` of its query and its significant tokens, operators merged."
  [matches merged text-lines measuring]
  (let [layout (token-layout merged)
        by-kind (group-by match-kind matches)
        scopes (class-scopes (:type by-kind) layout)
        private-scopes (into [] (keep #(brace-extent layout (:unnamed.namespace %))) (:unnamed.namespace by-kind))
        functions (function-entries by-kind layout scopes private-scopes)
        entries (sort-by (juxt :line :name)
                         (concat functions
                                 (named-entries :type clean-name (:type by-kind) layout scopes private-scopes)
                                 (named-entries :macro clean-name (:macro by-kind) layout [] [])
                                 (named-entries :value value-name (:value by-kind) layout [] private-scopes)))]
    {:definitions (mapv #(dissoc % :body) entries)
     :top-level-forms (top-level-forms (:form by-kind) layout text-lines entries)
     :measured (for [{[open close] :body :keys [kind line end-line] :as entry} functions
                     :when (= :function kind)]
                 [(dissoc entry :body)
                  (structure/measure-span (:c structure/rules) merged (inc open) close
                                          (structure/brace-nesting merged (:depths layout) open close)
                                          measuring line end-line)])
     :facts {:identifier-counts (identifier-counts merged)}}))

(defn read-file
  "Facts of a C or C++ file in `language-id`: its includes, the names it
  uses, and its definitions with their complection, from the syntax tree
  `parse-tree` answers queries about, when given."
  [language-id path text {:keys [parse-tree] :as options}]
  (let [normalized-text (source/normalize-text text)
        text-lines (source/split-lines normalized-text)
        language (file-language language-id path text-lines)
        matches (if parse-tree (parse-tree language normalized-text (get queries language)) [])]
    (structure/read-structured language path normalized-text options
                               (comp includes scanner/significant)
                               (fn [tokens _text-lines measuring]
                                 (tree-structure matches tokens text-lines measuring)))))

;; Pages

(def ^:private source-extensions #{"c" "cc" "cpp" "cxx" "c++"})
(def ^:private header-extensions #{"h" "hh" "hpp" "hxx" "h++"})

(def ^:private program-starts
  "The functions a program starts from."
  #{"main" "wmain" "WinMain"})

(defn- stem
  "A path's file name without its last extension."
  [path]
  (let [file-name (source/file-name path)
        dot-index (string/last-index-of file-name ".")]
    (if (and dot-index (pos? dot-index)) (subs file-name 0 dot-index) file-name)))

(defn- file-role
  ":source, :header, or :inline for a C or C++ file, by its extension."
  [path]
  (let [extension (source/file-extension path)]
    (cond
      (contains? source-extensions extension) :source
      (contains? header-extensions extension) :header
      :else :inline)))

(defn- in-folder-of
  "Those of `paths` in the same folder as `path`."
  [paths path]
  (filter #(= (source/directory-of %) (source/directory-of path)) paths))

(defn- stem-pairs
  "Map from a companion's path to the path whose page it joins, among
  files sharing one stem: the lone source in its folder, else the one
  source and one header left over anywhere. An inline file follows the
  header beside it."
  [paths]
  (let [{sources :source headers :header inlines :inline} (group-by file-role paths)
        beside-source (into {}
                            (keep (fn [companion]
                                    (let [beside (in-folder-of sources companion)]
                                      (when (= 1 (count beside)) [companion (first beside)]))))
                            (concat headers inlines))
        lone-sources (remove (set (vals beside-source)) sources)
        lone-headers (remove #(contains? beside-source %) headers)
        paired (if (and (= 1 (count lone-sources)) (= 1 (count lone-headers)))
                 (assoc beside-source (first lone-headers) (first lone-sources))
                 beside-source)]
    (into paired
          (keep (fn [inline]
                  (let [beside (in-folder-of headers inline)]
                    (when (and (not (contains? paired inline)) (= 1 (count beside)))
                      [inline (get paired (first beside) (first beside))]))))
          inlines)))

(defn- program?
  "True for a file defining a function a program starts from, which
  implements no header, as Lua's lua.c beside the library's lua.h."
  [{:keys [definitions]}]
  (boolean (some #(contains? program-starts (:name %)) definitions)))

(defn paired-units
  "File facts with each C or C++ header and inline file on the page of the
  source file it pairs with, as Board.hpp with Board.cpp, or
  include/filter.h with src/filter.c when no other filter source exists.
  A program keeps its own page."
  [file-facts]
  (let [paths (keep (fn [facts]
                      (when (and (contains? #{:c :cpp} (:language facts)) (not (program? facts)))
                        (:path facts)))
                    file-facts)
        unit-of (into {} (mapcat stem-pairs) (vals (group-by stem paths)))]
    (mapv (fn [facts]
            (if-let [unit-path (get unit-of (:path facts))]
              (assoc facts :unit-id unit-path)
              facts))
          file-facts)))
