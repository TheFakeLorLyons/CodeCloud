(ns codecloud.languages.shell
  "Reads shell scripts: the files they source and the project scripts they
  run, directly or through an interpreter. Also finds the commands
  Clojure and Babashka programs run."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure]))

(defn- separator?
  "True for a token that ends a word and the command it belongs to."
  [{:keys [kind text]}]
  (or (and (= :text kind) (contains? #{";" "&" "|"} text))
      (= :bracket kind)
      (and (= :reader kind) (contains? #{"$(" ")"} text))))

(defn- redirection? [{:keys [kind text]}]
  (and (= :text kind) (contains? #{">" "<"} text)))

(defn commands
  "Commands in a shell token stream, each a vector of unquoted {:text
  :line} words. Redirection targets and here documents are skipped."
  [tokens]
  (let [finish-word (fn [{:keys [word] :as state}]
                      (cond-> (assoc state :word nil)
                        (and word (not (:redirected? state)))
                        (update :command conj {:text (string/replace (:text word) #"[\"']" "") :line (:line word)})
                        word (assoc :redirected? false)))
        finish-command (fn [state]
                         (let [state (finish-word state)]
                           (cond-> (assoc state :command [])
                             (seq (:command state)) (update :found conj (:command state)))))]
    (:found
     (finish-command
      (reduce (fn [{:keys [word heredoc-line last-token] :as state} {:keys [kind text line] :as token}]
                (let [new-line? (and last-token (> line (:line last-token)) (not= "\\" (:text last-token)))
                      state (cond-> (assoc state :last-token token)
                              new-line? finish-command)]
                  (cond
                    (and heredoc-line (> line heredoc-line))
                    (if (= :reader kind) (assoc state :heredoc-line nil) state)

                    (and (= :reader kind) (string/starts-with? text "<<"))
                    (assoc (finish-command state) :heredoc-line line)

                    (or (= :space kind) (= :comment kind) (= "\\" text)) (finish-word state)
                    (separator? token) (finish-command state)
                    (redirection? token) (assoc (finish-word state) :redirected? true)
                    :else (assoc state :word (if (and word
                                                      (= line (:line word))
                                                      (= (:column token) (:end word)))
                                               (assoc word :text (str (:text word) text) :end (+ (:column token) (count text)))
                                               {:text text :line line :end (+ (:column token) (count text))})))))
              {:found [] :command [] :word nil :heredoc-line nil :last-token nil :redirected? false}
              tokens)))))

(def ^:private prefix-words
  "Words that may stand before the command they run."
  #{"sudo" "exec" "time" "nohup" "env" "command" "builtin" "then" "do" "else" "if" "elif" "while" "until" "!"})

(def ^:private interpreters
  "Programs that run a file given as an argument, by how they name it."
  {"bash" :path "sh" :path "zsh" :path "ksh" :path "dash" :path "source" :source "." :source
   "python" :python "python3" :python "python2" :python "pypy" :python "pypy3" :python
   "node" :path "deno" :path "bun" :path "ts-node" :path "tsx" :path "nbb" :path
   "ruby" :path "perl" :path "Rscript" :path "julia" :path "lua" :path "php" :path
   "bb" :clojure "clojure" :clojure "clj" :clojure})

(defn- assignment? [word]
  (re-matches #"[A-Za-z_][A-Za-z0-9_]*=.*" word))

(defn- option? [word]
  (string/starts-with? word "-"))

(defn- following [words flag]
  (second (drop-while #(not= flag %) words)))

(defn- run-of
  "{:specifier :resolver :kind} for the file one command runs or sources,
  or nil."
  [words]
  (let [[command-name & arguments] (drop-while #(or (contains? prefix-words %) (assignment? %)) words)
        operands (remove option? arguments)
        rule (get interpreters command-name)]
    (cond
      (nil? command-name) nil
      (= :source rule) (when-let [operand (first arguments)] {:specifier operand :resolver :path :kind :source})
      (= :python rule) (if-let [module (following arguments "-m")]
                         {:specifier module :resolver :python-module :kind :invoke}
                         (when-let [operand (first operands)] {:specifier operand :resolver :path :kind :invoke}))
      (= :clojure rule) (if-let [namespace-name (following arguments "-m")]
                          {:specifier namespace-name :resolver :clojure-namespace :kind :invoke}
                          (when-let [operand (or (following arguments "-f") (following arguments "--file")
                                                 (first (filter #(re-find #"\.(?:clj|cljc|bb)$" %) operands)))]
                            {:specifier operand :resolver :path :kind :invoke}))
      (= :path rule) (when-let [operand (first operands)]
                       (when-not (contains? #{"-c"} (first arguments))
                         {:specifier operand :resolver :path :kind :invoke}))
      (string/includes? command-name "/") {:specifier command-name :resolver :path :kind :invoke}
      :else nil)))

(defn imports-of-commands
  "The files a list of commands, from `commands`, sources and runs."
  [command-list]
  (into []
        (keep (fn [words]
                (when-let [run (run-of (mapv :text words))]
                  (assoc run :line (:line (first words)) :binds []))))
        command-list))

(defn command-imports
  "The files the command line `command-text` runs, as a program written
  in another language passes it to a shell, reported at `line`."
  [command-text line]
  (mapv #(assoc % :line line)
        (imports-of-commands (commands (scanner/token-stream :shell [command-text])))))

(defn read-script
  "Facts of a shell script: what it sources and runs, its functions, its
  top-level commands as forms, and its complection."
  [path text options]
  (structure/read-structured :shell path text options
                             (comp imports-of-commands commands)
                             structure/shell-structure))
