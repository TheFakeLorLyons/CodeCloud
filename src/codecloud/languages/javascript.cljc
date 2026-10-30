(ns codecloud.languages.javascript
  "Reads JavaScript and TypeScript files: their imports, re-exports,
  requires, and dynamic imports, each with the names it binds."
  (:require [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure]))

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
  from: a default name, * as a namespace, and braced names."
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
  "Names a require call binds: the module for `const x = require(...)`,
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

(defn- import-at
  "The import a statement starting at `index` makes, or nil."
  [tokens index]
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
  "Facts of a JavaScript or TypeScript file in `language-id`: its imports,
  definitions, top-level forms, and complection."
  [language-id path text options]
  (structure/read-structured language-id path text options
                             (comp imports scanner/significant)
                             structure/script-structure))
