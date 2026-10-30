(ns codecloud.languages.python
  "Reads Python imports, as dotted names with any leading relative dots,
  and the names each binds."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure]))

(defn- statement-start?
  "True when the token at `index` begins a logical line."
  [tokens index]
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
  "Facts of a Python file: its imports, definitions, top-level forms, and
  complection."
  [path text options]
  (structure/read-structured :python path text options
                             (comp imports scanner/significant)
                             structure/python-structure))
