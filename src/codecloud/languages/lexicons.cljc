(ns codecloud.languages.lexicons
  "Each language's lexicon for codecloud.languages.lexer, by id. Token
  kinds are :text :comment :string :keyword :number :constant :special
  :definition :reader :bracket :discard and :space. Reserved words are
  :special, defined names :definition, builtin types and property names
  :keyword, and directives, decorators, and sigil variables :reader."
  (:require [clojure.string :as string]))

(defn- word-set
  "The words of a space-separated string, as a set."
  [text]
  (set (string/split (string/trim text) #"\s+")))

(defn- any-case
  "A regex fragment matching `word` in any letter case, written with
  character classes so it reads the same on the JVM and in JavaScript."
  [word]
  (apply str (map (fn [character]
                    (let [letter (str character)]
                      (if (= (string/lower-case letter) (string/upper-case letter))
                        letter
                        (str "[" (string/lower-case letter) (string/upper-case letter) "]"))))
                  word)))

(def ^:private spacing [:space #"^\s+"])
(def ^:private brackets [:bracket #"^[\(\)\[\]\{\}]"])

(def ^:private c-number
  "Decimal, hexadecimal, octal, and binary numbers with an optional
  exponent and any type suffix, such as 0xFFu, 1.5e-3f, or 10_000n."
  [:number #"^(?:0[xX][0-9a-fA-F_]+|0[bB][01_]+|0[oO][0-7_]+|(?:\d[\d_]*(?:\.[\d_]*)?|\.\d[\d_]*)(?:[eEpP][+-]?\d+)?)[A-Za-z]*"])

(def ^:private block-comment {:open "/*" :close "*/" :kind :comment})
(def ^:private line-comment {:open "//" :close nil :kind :comment})
(def ^:private double-quoted {:open "\"" :close "\"" :escape "\\" :kind :string :single-line? true})
(def ^:private single-quoted {:open "'" :close "'" :escape "\\" :kind :string :single-line? true})
(def ^:private multi-line-double-quoted {:open "\"" :close "\"" :escape "\\" :kind :string})
(def ^:private multi-line-single-quoted {:open "'" :close "'" :escape "\\" :kind :string})

(defn- reserved-word-kind
  "Kinds for a language's words: :constant, :special, :definition after a
  defining word, :keyword for builtin types, and :text for members after
  a dot, unless the language reserves those, as Python does."
  [{:keys [reserved constants defining types reserved-after-dot?]}]
  (fn [word previous _line _end]
    (let [[previous-kind previous-text] previous]
      (cond
        (and (= [:text "."] previous) (not reserved-after-dot?)) :text
        (contains? constants word) :constant
        (contains? reserved word) :special
        (and (= :special previous-kind) (contains? defining previous-text)) :definition
        (contains? types word) :keyword
        :else :text))))

(defn- lexicon
  "A lexicon for a language with reserved words, from its spans, the
  patterns tried before the shared ones, and its word sets."
  [{:keys [spans patterns words word-pattern]}]
  {:spans spans
   :patterns (into (vec patterns)
                   [spacing c-number brackets [:word (or word-pattern #"^[A-Za-z_][A-Za-z0-9_]*")]])
   :word-kind (reserved-word-kind words)
   :insignificant #{:space :comment}})

;; Clojure

(def ^:private clojure-special-names
  #{"def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defonce" "defprotocol"
    "defrecord" "deftype" "definterface" "defstruct" "deftest" "defspec"
    "let" "letfn" "fn" "fn*" "if" "if-let" "if-not" "if-some" "when" "when-let"
    "when-not" "when-some" "when-first" "cond" "condp" "case" "do" "loop" "recur"
    "try" "catch" "finally" "throw" "doseq" "dotimes" "for" "ns" "require" "import"
    "quote" "var" "new" "set!" "and" "or" "->" "->>" "as->" "cond->" "cond->>"
    "some->" "some->>" "binding" "with-open" "doto" "comment" "declare" "reify"
    "proxy" "extend-type" "extend-protocol" "in-ns"})

(defn- clojure-word-kind
  "Kind of a Clojure symbol by its role, :special at the head of a special
  form, :definition for the name a def form defines, :constant, or :text."
  [word previous-significant _line _end]
  (let [[previous-kind previous-text] previous-significant]
    (cond
      (contains? #{"true" "false" "nil"} word) :constant
      (and (= :special previous-kind) (string/starts-with? previous-text "def")) :definition
      (and (= [:bracket "("] previous-significant)
           (or (contains? clojure-special-names word) (string/starts-with? word "def")))
      :special
      :else :text)))

(def clojure
  {:spans [{:open "\"" :close "\"" :escape "\\" :kind :string}
           {:open "#\"" :close "\"" :escape "\\" :kind :string}]
   :patterns [[:space #"^[\s,]+"]
              [:comment #"^;.*"]
              [:constant #"^##(?:Inf|-Inf|NaN)"]
              [:discard #"^#_"]
              [:bracket #"^#[\{\(]"]
              [:reader #"^#(?:\?@|\?|'|=|:[^\s,\(\[\{]*)"]
              [:reader #"^#[a-zA-Z][^\s,\(\)\[\]\{\}\"]*"]
              [:bracket #"^[\(\)\[\]\{\}]"]
              [:string #"^\\(?:newline|space|tab|formfeed|backspace|return|u[0-9a-fA-F]{4}|o[0-7]{1,3}|.)"]
              [:reader #"^(?:~@|['`~@\^])"]
              [:keyword #"^::?[^\s,\(\)\[\]\{\}\"';@\^`~\\]+"]
              [:number #"^[-+]?\d[^\s,\(\)\[\]\{\}\"';]*"]
              [:word #"^[^\s,\(\)\[\]\{\}\"';@\^`~\\]+"]]
   :word-kind clojure-word-kind
   :insignificant #{:space :comment}})

;; Python

(def python
  (lexicon
   {:spans [{:open "\"\"\"" :close "\"\"\"" :escape "\\" :kind :string}
            {:open "'''" :close "'''" :escape "\\" :kind :string}
            double-quoted
            single-quoted]
    :patterns [[:comment #"^#.*"]
               [:string #"^(?:[rR][bBfF]?|[bBfF][rR]?|[uU])(?=[\"'])"]
               [:reader #"^@[A-Za-z_][\w.]*"]
               [:number #"^(?:0[xX][0-9a-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+|(?:\d[\d_]*(?:\.[\d_]*)?|\.\d[\d_]*)(?:[eE][+-]?\d+)?[jJ]?)"]]
    :words {:reserved (word-set "and as assert async await break class continue def del elif else except
                                 finally for from global if import in is lambda nonlocal not or pass raise
                                 return try while with yield")
            :constants (word-set "True False None")
            :defining #{"def" "class"}
            :types #{}
            :reserved-after-dot? true}}))

;; JavaScript and TypeScript

(def ^:private words-before-expressions
  "Keywords after which a slash starts a regular expression."
  (word-set "return typeof instanceof in of new delete void throw case do else yield await"))

(defn- regex-may-open?
  "True when a slash after `previous` starts a regex: at the start, after
  an operator or opening bracket, or after a keyword such as return."
  [previous]
  (let [[previous-kind previous-text] previous]
    (or (nil? previous)
        (and (= :bracket previous-kind) (contains? #{"(" "[" "{"} previous-text))
        (and (= :text previous-kind) (not (re-find #"^[A-Za-z0-9_$]" previous-text)))
        (and (= :special previous-kind) (contains? words-before-expressions previous-text)))))

(def ^:private javascript-spans
  [block-comment
   line-comment
   {:open "/" :close "/" :escape "\\" :kind :string :single-line? true :allowed? regex-may-open?}
   {:open "`" :close "`" :escape "\\" :kind :string}
   double-quoted
   single-quoted])

(def ^:private javascript-patterns
  [[:comment #"^#!.*"]
   [:reader #"^@[A-Za-z_$][\w$.]*"]
   [:text #"^#[A-Za-z_$][\w$]*"]])

(def ^:private javascript-reserved
  "break case catch class const continue debugger default delete do else export extends finally for
   function if import in instanceof let new return switch throw try typeof var void while with yield
   async await of from as static")

(def javascript
  (lexicon
   {:spans javascript-spans
    :patterns javascript-patterns
    :word-pattern #"^[A-Za-z_$][A-Za-z0-9_$]*"
    :words {:reserved (word-set javascript-reserved)
            :constants (word-set "true false null undefined NaN Infinity this super")
            :defining #{"function" "class"}
            :types #{}}}))

(def typescript
  (lexicon
   {:spans javascript-spans
    :patterns javascript-patterns
    :word-pattern #"^[A-Za-z_$][A-Za-z0-9_$]*"
    :words {:reserved (word-set (str javascript-reserved
                                     " interface type enum namespace declare abstract implements private"
                                     " protected public readonly keyof infer is satisfies asserts module"
                                     " override unique"))
            :constants (word-set "true false null undefined NaN Infinity this super")
            :defining #{"function" "class" "interface" "type" "enum" "namespace"}
            :types (word-set "string number boolean any unknown never void object symbol bigint")}}))

;; CSS, SCSS, and Less

(defn- css-line-comment-may-open?
  "True when // starts an SCSS or Less comment, where a rule or
  declaration may begin, so http://example.com stays a value."
  [previous]
  (or (nil? previous) (contains? #{[:bracket "{"] [:bracket "}"] [:text ";"]} previous)))

(def ^:private css-spans
  [block-comment
   {:open "//" :close nil :kind :comment :allowed? css-line-comment-may-open?}
   double-quoted
   single-quoted])

(defn- selector-may-start?
  "True when .name or #name after `previous` starts a selector, which it
  cannot right after a word, as in example.com or math.div."
  [previous]
  (let [[previous-kind previous-text] previous]
    (not (and (= :text previous-kind) (re-find #"^[\w-]" previous-text)))))

(def ^:private css-patterns
  [spacing
   [:constant #"^#[0-9a-fA-F]{3,8}(?![\w-])"]
   [:definition #"^[.#][A-Za-z_-][\w-]*" selector-may-start?]
   [:special #"^@[A-Za-z_-][\w-]*"]
   [:reader #"^\$[A-Za-z_-][\w-]*"]
   [:reader #"^!\s*[A-Za-z]+"]
   [:reader #"^&"]
   [:number #"^-?(?:\d+\.?\d*|\.\d+)(?:%|[A-Za-z]+)?"]])

(defn- css-declaration-word-kind
  "Inside a block, a word before a colon is a property unless a brace
  follows on the line, as in a:hover. Other words are values."
  [word previous line end]
  (let [rest-of-line (subs line end)
        declaration-start? (or (nil? previous) (contains? #{[:bracket "{"] [:bracket "}"] [:text ";"]} previous))]
    (cond
      (and declaration-start? (re-find #"^\s*:[^;{]*\{" rest-of-line)) :definition
      (and declaration-start? (re-find #"^\s*:(?!:)" rest-of-line)) :keyword
      (and declaration-start? (re-find #"^[^;:]*\{" rest-of-line)) :definition
      :else (if (contains? #{"inherit" "initial" "unset" "none" "auto"} word) :constant :text))))

(def ^:private css-block-region
  {:open #"^\{" :close #"^\}" :open-kind :bracket :close-kind :bracket})

(def css-block
  "Declarations inside braces, which may hold nested rules."
  {:spans css-spans
   :regions [(assoc css-block-region :lexicon :self)]
   :patterns (conj css-patterns [:word #"^[A-Za-z_-][\w-]*"])
   :word-kind css-declaration-word-kind
   :insignificant #{:space :comment}})

(def css
  {:spans css-spans
   :regions [(assoc css-block-region :lexicon css-block)]
   :patterns (conj css-patterns [:word #"^[A-Za-z_*-][\w-]*"])
   :word-kind (fn [word _previous _line _end]
                (if (contains? #{"and" "or" "not" "only" "from" "to"} word) :special :definition))
   :insignificant #{:space :comment}})

;; HTML

(def ^:private html-tag
  "The inside of a tag: its name first, then attribute names, each with
  an optional value."
  {:spans [{:open "\"" :close "\"" :kind :string}
           {:open "'" :close "'" :kind :string}]
   :patterns [spacing
              [:text #"^="]
              [:word #"^[^\s=/>\"']+"]]
   :word-kind (fn [_word previous _line _end]
                (cond
                  (nil? previous) :special
                  (= [:text "="] previous) :string
                  :else :keyword))
   :insignificant #{:space}})

(defn- embedded-region
  "The region a tag starts, which is the script after <script> and the
  style sheet after <style>, each running up to its closing tag."
  [opening tag-name]
  (when (= "<" opening)
    (case (string/lower-case (or tag-name ""))
      "script" {:lexicon javascript :close (re-pattern (str "^</" (any-case "script"))) :close-before? true}
      "style" {:lexicon css :close (re-pattern (str "^</" (any-case "style"))) :close-before? true}
      nil)))

(def html
  {:spans [{:open "<!--" :close "-->" :kind :comment}
           {:open "<!" :close ">" :kind :reader}
           {:open "<?" :close "?>" :kind :reader}]
   :regions [{:open #"^</?(?=[A-Za-z])"
              :close #"^/?>"
              :lexicon html-tag
              :open-kind :bracket
              :close-kind :bracket
              :then embedded-region}]
   :patterns [spacing
              [:constant #"^&(?:#\d+|#[xX][0-9a-fA-F]+|[A-Za-z]\w*);"]
              [:text #"^[^\s<&]+"]]
   :word-kind (fn [_word _previous _line _end] :text)
   :insignificant #{:space :comment}})

;; Shell

(def ^:private shell-reserved
  (word-set "if then else elif fi case esac for select while until do done in function time coproc"))

(def ^:private shell-builtins
  (word-set "echo cd export local readonly declare typeset unset source alias set shift return exit
             eval exec trap wait read printf test let shopt pushd popd getopts command builtin"))

(defn- shell-word-kind
  "A reserved word is :special and a builtin :keyword. A word followed by
  () or following the word function names a function."
  [word previous line end]
  (let [[previous-kind previous-text] previous]
    (cond
      (contains? #{"true" "false"} word) :constant
      (and (= :special previous-kind) (= "function" previous-text)) :definition
      (re-find #"^\s*\(\s*\)" (subs line end)) :definition
      (contains? shell-reserved word) :special
      (contains? shell-builtins word) :keyword
      :else :text)))

(def ^:private heredoc-body
  {:spans []
   :patterns [[:string #"^.+"]]
   :word-kind (fn [_word _previous _line _end] :string)
   :insignificant #{}})

(declare shell)

(def ^:private shell-variables
  [[:reader #"^\$\{[^}]*\}"]
   [:reader #"^\$[A-Za-z_][A-Za-z0-9_]*"]
   [:reader #"^\$[#?$!@*0-9-]"]])

(def ^:private command-substitution
  "$( ... ), whose commands read as shell anywhere, inside a string too."
  {:open #"^\$\(" :close #"^\)" :lexicon (fn [] shell) :open-kind :reader :close-kind :reader})

(def ^:private shell-double-quoted
  "The inside of a double-quoted string, where variables and command
  substitutions still expand."
  {:spans []
   :regions [command-substitution]
   :patterns (into [[:string #"^\\."]]
                   (conj shell-variables
                         [:string #"^[^\"\\$]+"]
                         [:string #"^\$"]))
   :word-kind (fn [_word _previous _line _end] :string)
   :insignificant #{}})

(def shell
  {:spans [{:open "'" :close "'" :kind :string}
           {:open "`" :close "`" :escape "\\" :kind :string}]
   :regions [{:open #"^\"" :close #"^\"" :lexicon shell-double-quoted :open-kind :string :close-kind :string}
             command-substitution
             {:open #"^<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1"
              :close-for (fn [found] (re-pattern (str "^\\s*" (nth found 2) "\\s*$")))
              :next-line? true
              :lexicon heredoc-body
              :open-kind :reader
              :close-kind :reader}]
   :patterns (into [spacing [:comment #"^#.*"]]
                   (concat shell-variables
                           [[:text #"^\\.?"]
                            [:number #"^\d+(?![\w./-])"]
                            [:keyword #"^--?[A-Za-z][\w-]*"]
                            brackets
                            [:word #"^[^\s;&|<>(){}\[\]\"'`$\\#][^\s;&|<>(){}\[\]\"'`$\\]*"]]))
   :word-kind shell-word-kind
   :insignificant #{:space :comment}})

;; The C family and other brace languages

(defn- after-include?
  "True right after #include or #import, where <file> names a header."
  [previous]
  (let [[previous-kind previous-text] previous]
    (and (= :reader previous-kind) (re-find #"^#\s*(?:include|import)" previous-text))))

(def ^:private c-reserved
  "auto break case const continue default do else enum extern for goto if inline register restrict
   return sizeof static struct switch typedef union volatile while _Alignas _Alignof _Atomic _Generic
   _Noreturn _Static_assert _Thread_local")

(def ^:private c-types
  "char double float int long short signed unsigned void bool _Bool _Complex size_t ssize_t ptrdiff_t
   int8_t int16_t int32_t int64_t uint8_t uint16_t uint32_t uint64_t intptr_t uintptr_t FILE")

(def ^:private c-family-number
  "Numbers whose digits C23 and C++14 may group with apostrophes, such as
  1'000'000 or 0xFF'FF, which would otherwise open a character literal."
  [:number #"^(?:0[xX][0-9a-fA-F](?:'?[0-9a-fA-F])*|0[bB][01](?:'?[01])*|\d(?:'?\d)*(?:\.(?:\d(?:'?\d)*)?)?(?:[eEpP][+-]?\d+)?)[A-Za-z]*"])

(def ^:private c-patterns
  [[:reader #"^#\s*[A-Za-z_]+"]
   [:string #"^<[^<>\s]+>" after-include?]
   c-family-number])

(defn- regex-quoted
  "`text` with each character a regex treats specially escaped."
  [text]
  (string/replace text #"[\\^$.|?*+()\[\]{}]" (fn [character] (str "\\" character))))

(def ^:private raw-string-body
  "The inside of a C++ raw string, read up to each closing parenthesis so
  the region can test it against the delimiter."
  {:spans []
   :patterns [[:string #"^[^)]+"] [:string #"^\)"]]
   :word-kind (fn [_word _previous _line _end] :string)
   :insignificant #{}})

(def ^:private raw-string
  "A C++ raw string such as R\"(...)\" or R\"sql(...)sql\", which runs to a
  parenthesis followed by its delimiter, across lines if need be."
  {:open #"^(?:u8|u|U|L)?R\"([^()\\\s\"]{0,16})\("
   :close-for (fn [found] (re-pattern (str "^\\)" (regex-quoted (second found)) "\"")))
   :lexicon raw-string-body
   :open-kind :string
   :close-kind :string})

(def c
  (lexicon
   {:spans [block-comment line-comment double-quoted single-quoted]
    :patterns c-patterns
    :words {:reserved (word-set c-reserved)
            :constants (word-set "NULL true false")
            :defining (word-set "struct enum union")
            :types (word-set c-types)}}))

(def cpp
  (assoc
   (lexicon
    {:spans [block-comment line-comment double-quoted single-quoted]
    :patterns c-patterns
    :words {:reserved (word-set (str c-reserved
                                     " alignas alignof asm catch class concept consteval constexpr constinit"
                                     " const_cast co_await co_return co_yield decltype delete dynamic_cast"
                                     " explicit export final friend mutable namespace new noexcept operator"
                                     " override private protected public reinterpret_cast requires"
                                     " static_assert static_cast template thread_local throw try typeid"
                                     " typename using virtual and or not xor"))
            :constants (word-set "nullptr NULL true false this")
            :defining (word-set "struct enum union class namespace concept")
            :types (word-set (str c-types " wchar_t char8_t char16_t char32_t auto"))}})
   :regions [raw-string]))

(def java
  (lexicon
   {:spans [block-comment line-comment
            {:open "\"\"\"" :close "\"\"\"" :escape "\\" :kind :string}
            double-quoted single-quoted]
    :patterns [[:reader #"^@[A-Za-z_][\w.]*"]]
    :words {:reserved (word-set "abstract assert break case catch class const continue default do else enum
                                 extends final finally for goto if implements import instanceof interface
                                 native new package private protected public return static strictfp switch
                                 synchronized throw throws transient try volatile while var record sealed
                                 permits yield")
            :constants (word-set "true false null this super")
            :defining (word-set "class interface enum record")
            :types (word-set "boolean byte char double float int long short void")}}))

(def kotlin
  (lexicon
   {:spans [block-comment line-comment
            {:open "\"\"\"" :close "\"\"\"" :kind :string}
            double-quoted single-quoted]
    :patterns [[:reader #"^@[A-Za-z_][\w.:]*"]]
    :words {:reserved (word-set "as break class continue do else for fun if in interface is object package
                                 return throw try typealias val var when while by catch constructor finally
                                 import init where abstract annotation companion const data enum external
                                 final infix inline inner internal lateinit open operator out override
                                 private protected public reified sealed suspend tailrec vararg")
            :constants (word-set "true false null this super")
            :defining (word-set "class interface object fun typealias")
            :types (word-set "Int Long Short Byte Double Float Boolean Char String Unit Any Nothing")}}))

(def csharp
  (lexicon
   {:spans [block-comment line-comment
            {:open "@\"" :close "\"" :kind :string}
            double-quoted single-quoted]
    :patterns [[:reader #"^#\s*[A-Za-z]+"]]
    :words {:reserved (word-set "abstract as base break case catch checked class const continue default
                                 delegate do else enum event explicit extern finally fixed for foreach goto
                                 if implicit in interface internal is lock namespace new operator out
                                 override params private protected public readonly ref return sealed
                                 sizeof stackalloc static struct switch throw try typeof unchecked unsafe
                                 using virtual volatile while async await record yield partial where")
            :constants (word-set "true false null this")
            :defining (word-set "class interface enum struct record namespace")
            :types (word-set "bool byte char decimal double float int long object sbyte short string uint
                              ulong ushort void var dynamic")}}))

(def go
  (lexicon
   {:spans [block-comment line-comment
            {:open "`" :close "`" :kind :string}
            double-quoted single-quoted]
    :words {:reserved (word-set "break case chan const continue default defer else fallthrough for func go
                                 goto if import interface map package range return select struct switch
                                 type var")
            :constants (word-set "true false nil iota")
            :defining (word-set "func type")
            :types (word-set "bool byte complex64 complex128 error float32 float64 int int8 int16 int32
                              int64 rune string uint uint8 uint16 uint32 uint64 uintptr any")}}))

(def rust
  (lexicon
   {:spans [block-comment line-comment multi-line-double-quoted]
    :patterns [[:string #"^'(?:\\u\{[0-9a-fA-F]+\}|\\.|[^\\'])'"]
               [:reader #"^'[A-Za-z_]\w*"]
               [:reader #"^#!?\["]
               [:reader #"^[A-Za-z_]\w*!(?=\s*[\(\[\{])"]]
    :words {:reserved (word-set "as async await break const continue crate dyn else enum extern fn for if
                                 impl in let loop match mod move mut pub ref return static struct super
                                 trait type unsafe use where while union")
            :constants (word-set "true false self Self")
            :defining (word-set "fn struct enum trait mod type union")
            :types (word-set "i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f32 f64 bool char str")}}))

(def ruby
  (lexicon
   {:spans [multi-line-double-quoted
            multi-line-single-quoted
            {:open "/" :close "/" :escape "\\" :kind :string :single-line? true :allowed? regex-may-open?}]
    :patterns [[:comment #"^#.*"]
               [:keyword #"^:[A-Za-z_]\w*[?!]?"]
               [:reader #"^@@?[A-Za-z_]\w*"]
               [:reader #"^\$[A-Za-z_]\w*"]]
    :word-pattern #"^[A-Za-z_]\w*[?!]?"
    :words {:reserved (word-set "alias and begin BEGIN break case class def defined? do else elsif end END
                                 ensure for if in module next not or redo rescue retry return super then
                                 undef unless until when while yield require require_relative include
                                 extend raise lambda proc attr_accessor attr_reader attr_writer private
                                 protected public")
            :constants (word-set "true false nil self __FILE__ __LINE__")
            :defining (word-set "def class module")
            :types #{}}}))

(def php
  (lexicon
   {:spans [block-comment line-comment multi-line-double-quoted multi-line-single-quoted]
    :regions [{:open #"^<<<\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1"
               :close-for (fn [found] (re-pattern (str "^\\s*" (nth found 2) "\\b")))
               :next-line? true
               :lexicon heredoc-body
               :open-kind :reader
               :close-kind :reader}]
    :patterns [[:reader #"^<\?(?:php|=)?"]
               [:reader #"^\?>"]
               [:comment #"^#(?!\[).*"]
               [:reader #"^\$[A-Za-z_]\w*"]
               [:reader #"^#\["]]
    :words {:reserved (word-set "abstract and array as break callable case catch class clone const continue
                                 declare default do echo else elseif empty enum extends final finally fn for
                                 foreach function global goto if implements include include_once instanceof
                                 insteadof interface isset list match namespace new or print private
                                 protected public readonly require require_once return static switch throw
                                 trait try unset use var while xor yield")
            :constants (word-set "true false null TRUE FALSE NULL")
            :defining (word-set "function class interface trait enum")
            :types (word-set "int float bool string void mixed never object iterable")}}))

(def lua
  (lexicon
   {:spans [{:open "--[[" :close "]]" :kind :comment}
            {:open "--" :close nil :kind :comment}
            {:open "[[" :close "]]" :kind :string}
            double-quoted
            single-quoted]
    :words {:reserved (word-set "and break do else elseif end for function goto if in local not or repeat
                                 return then until while")
            :constants (word-set "true false nil self")
            :defining #{"function"}
            :types #{}}}))

(defn- r-word-kind
  "R names a function by assigning one, so a word followed by <- function
  or = function is a definition."
  [word previous line end]
  (if (re-find #"^\s*(?:<<?-|=)\s*function\b" (subs line end))
    :definition
    ((reserved-word-kind {:reserved (word-set "if else repeat while function for in next break return
                                                library require")
                          :constants (word-set "TRUE FALSE NULL Inf NaN NA NA_integer_ NA_real_ NA_character_")
                          :defining #{}
                          :types #{}})
     word previous line end)))

(def r
  {:spans [multi-line-double-quoted
           multi-line-single-quoted
           {:open "`" :close "`" :kind :text}]
   :patterns [spacing
              [:comment #"^#.*"]
              c-number
              brackets
              [:word #"^[A-Za-z.][A-Za-z0-9._]*"]]
   :word-kind r-word-kind
   :insignificant #{:space :comment}})

(def julia
  (lexicon
   {:spans [{:open "#=" :close "=#" :kind :comment}
            {:open "\"\"\"" :close "\"\"\"" :escape "\\" :kind :string}
            multi-line-double-quoted]
    :patterns [[:comment #"^#.*"]
               [:string #"^'(?:\\.|[^\\'])'"]
               [:keyword #"^:[A-Za-z_]\w*"]
               [:reader #"^@[A-Za-z_][\w.]*"]]
    :word-pattern #"^[A-Za-z_]\w*!?"
    :words {:reserved (word-set "abstract baremodule begin break catch const continue do else elseif end
                                 export finally for function global if import in isa let local macro module
                                 mutable primitive quote return struct try type using where while")
            :constants (word-set "true false nothing missing")
            :defining (word-set "function struct module macro")
            :types #{}}}))

(def plain
  "Text with no known syntax, split into words and spaces."
  {:spans []
   :patterns [spacing [:text #"^\S+"]]
   :word-kind (fn [_word _previous _line _end] :text)
   :insignificant #{:space}})

(def by-language
  {:clojure clojure
   :python python
   :javascript javascript
   :typescript typescript
   :css css
   :html html
   :shell shell
   :c c
   :cpp cpp
   :java java
   :kotlin kotlin
   :csharp csharp
   :go go
   :rust rust
   :ruby ruby
   :php php
   :lua lua
   :r r
   :julia julia})

(defn lexicon-for
  "The lexicon of `language-id`, or the plain one for a language without
  its own."
  [language-id]
  (get by-language language-id plain))
