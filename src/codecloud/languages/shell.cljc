(ns codecloud.languages.shell
  "Reads shell scripts for their structure, found by keywords, and for
  the files they source and the project scripts they run, directly or
  through an interpreter. Also finds the commands Clojure and Babashka
  programs run."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure
             :refer [first-index form-span matching-close measure-span rules]]))

;; Structure, by keywords

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

(defn- shell-structure
  "{:definitions :top-level-forms :measured} of a shell script's
  significant tokens, operators merged."
  [tokens text-lines measuring]
  (let [depths (structure/depths shell-opener? shell-closer? tokens)
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


;; Imports

(defn- separator? [{:keys [kind text]}]
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

(defn imports-of-commands [command-list]
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
  "Facts of a shell script, which are what it sources and runs, its
  functions, its top-level commands as forms, and its complection."
  [path text options]
  (structure/read-structured :shell path text options
                             (comp imports-of-commands commands)
                             shell-structure))
