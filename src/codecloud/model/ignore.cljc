(ns codecloud.model.ignore
  "Gitignore-style patterns for cloud file ignore lists and .gitignore
  files outside git. A rule set {:base :rules} applies under its base,
  \"\" being the root. Paths are relative, with forward slashes."
  (:require [clojure.string :as string]))

(defn- one-character [text index]
  (subs text index (inc index)))

(defn- escape-regex-character [character]
  (if (string/includes? ".^$|()[]{}+*?\\" character)
    (str "\\" character)
    character))

(defn- bracket-expression
  "[regex next-index] for the bracket expression at `open-index`, or nil
  when it never closes."
  [segment open-index]
  (let [after-open (inc open-index)
        negated? (and (< after-open (count segment))
                      (contains? #{"!" "^"} (one-character segment after-open)))
        content-start (if negated? (inc after-open) after-open)
        literal-close-first? (and (< content-start (count segment))
                                  (= "]" (one-character segment content-start)))
        search-from (if literal-close-first? (inc content-start) content-start)
        close-index (string/index-of segment "]" search-from)]
    (when close-index
      (let [content (subs segment content-start close-index)
            escaped-content (-> content
                                (string/replace "\\" "\\\\")
                                (string/replace "[" "\\[")
                                (string/replace "]" "\\]")
                                (string/replace "^" "\\^"))]
        [(str "[" (when negated? "^") escaped-content "]") (inc close-index)]))))

(defn- segment-regex
  "Regex for one slash-free piece of a pattern."
  [segment]
  (loop [index 0
         fragments []]
    (if (>= index (count segment))
      (apply str fragments)
      (let [character (one-character segment index)]
        (case character
          "*" (let [run-end (loop [run-index index]
                              (if (and (< run-index (count segment))
                                       (= "*" (one-character segment run-index)))
                                (recur (inc run-index))
                                run-index))]
                (recur run-end (conj fragments "[^/]*")))
          "?" (recur (inc index) (conj fragments "[^/]"))
          "[" (if-let [[expression next-index] (bracket-expression segment index)]
                (recur next-index (conj fragments expression))
                (recur (inc index) (conj fragments "\\[")))
          "\\" (if (< (inc index) (count segment))
                 (recur (+ index 2)
                        (conj fragments (escape-regex-character (one-character segment (inc index)))))
                 (recur (inc index) (conj fragments "\\\\")))
          (recur (inc index) (conj fragments (escape-regex-character character))))))))

(defn- pattern-body-regex
  "Regex for the slash-separated pattern body, giving `**` its gitignore
  meaning of any number of directories."
  [body]
  (let [segments (string/split body #"/" -1)
        last-index (dec (count segments))]
    (apply str
           (map-indexed (fn [segment-index segment]
                          (cond
                            (and (= "**" segment) (= segment-index last-index)) ".*"
                            (= "**" segment) "(?:.*/)?"
                            (= segment-index last-index) (segment-regex segment)
                            :else (str (segment-regex segment) "/")))
                        segments))))

(defn- trim-unescaped-trailing-spaces [line]
  (loop [trimmed line]
    (if (and (string/ends-with? trimmed " ")
             (not (string/ends-with? trimmed "\\ ")))
      (recur (subs trimmed 0 (dec (count trimmed))))
      trimmed)))

(defn compile-pattern
  "Rule for one gitignore line, or nil for a blank line or comment."
  [line]
  (let [trimmed (trim-unescaped-trailing-spaces line)]
    (when-not (or (string/blank? trimmed) (string/starts-with? trimmed "#"))
      (let [negated? (string/starts-with? trimmed "!")
            unnegated (if negated? (subs trimmed 1) trimmed)
            unescaped (if (or (string/starts-with? unnegated "\\#")
                              (string/starts-with? unnegated "\\!"))
                        (subs unnegated 1)
                        unnegated)
            directory-only? (string/ends-with? unescaped "/")
            without-trailing-slash (if directory-only?
                                     (subs unescaped 0 (dec (count unescaped)))
                                     unescaped)
            anchored? (string/includes? without-trailing-slash "/")
            body (if (string/starts-with? without-trailing-slash "/")
                   (subs without-trailing-slash 1)
                   without-trailing-slash)]
        (when-not (string/blank? body)
          {:pattern line
           :negated? negated?
           :directory-only? directory-only?
           :regex (re-pattern (str "^"
                                   (when-not anchored? "(?:.*/)?")
                                   (pattern-body-regex body)
                                   "$"))})))))

(defn rule-set
  "Rule set for a sequence of pattern lines that apply under `base`."
  [base pattern-lines]
  {:base base
   :rules (into [] (keep compile-pattern) pattern-lines)})

(defn rule-set-from-text
  "Rule set for the text of a .gitignore file found in directory `base`."
  [base text]
  (rule-set base (string/split-lines text)))

(defn- path-relative-to [base path]
  (cond
    (= "" base) path
    (string/starts-with? path (str base "/")) (subs path (inc (count base)))
    :else nil))

(defn- verdict
  "Whether `path` is ignored after every applicable rule has had its say;
  the last matching rule wins, as in git."
  [rule-sets path directory?]
  (reduce (fn [ignored-so-far {:keys [base rules]}]
            (if-let [relative-path (path-relative-to base path)]
              (reduce (fn [inner-verdict rule]
                        (if (and (or directory? (not (:directory-only? rule)))
                                 (re-matches (:regex rule) relative-path))
                          (not (:negated? rule))
                          inner-verdict))
                      ignored-so-far
                      rules)
              ignored-so-far))
          false
          rule-sets))

(defn- ancestor-directories [path]
  (let [segments (string/split path #"/")]
    (map #(string/join "/" (take % segments)) (range 1 (count segments)))))

(defn- ignored-as?
  "True when `path`, a folder when `directory?`, is ignored, which an
  ignored ancestor folder settles, as in git."
  [rule-sets path directory?]
  (boolean (or (some #(verdict rule-sets % true) (ancestor-directories path))
               (verdict rule-sets path directory?))))

(defn ignored?
  "True when the file at `path` is ignored."
  [rule-sets path]
  (ignored-as? rule-sets path false))

(defn directory-ignored?
  "True when the folder at `path` is ignored."
  [rule-sets path]
  (ignored-as? rule-sets path true))
