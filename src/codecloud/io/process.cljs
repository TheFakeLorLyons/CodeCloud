(ns codecloud.io.process
  "Runs child processes such as git."
  (:require ["child_process" :as child-process]))

(defn run
  "Runs `executable` in `directory`, resolving to {:exit-code :stdout
  :stderr} whatever the exit code; :stdout is a Buffer when `binary?`."
  [executable arguments {:keys [directory input binary?]}]
  (js/Promise.
   (fn [resolve reject]
     (let [^js child (.spawn child-process executable (into-array arguments)
                             #js {:cwd directory
                                  :windowsHide true
                                  :stdio #js ["pipe" "pipe" "pipe"]})
           stdout-chunks (array)
           stderr-chunks (array)]
       (.on (.-stdout child) "data" (fn [chunk] (.push stdout-chunks chunk)))
       (.on (.-stderr child) "data" (fn [chunk] (.push stderr-chunks chunk)))
       (.on child "error" reject)
       (.on child "close"
            (fn [exit-code]
              (let [stdout-buffer (js/Buffer.concat stdout-chunks)]
                (resolve {:exit-code exit-code
                          :stdout (if binary? stdout-buffer (.toString stdout-buffer "utf8"))
                          :stderr (.toString (js/Buffer.concat stderr-chunks) "utf8")}))))
       (.on (.-stdin child) "error" (fn [_error] nil))
       (if input
         (.end (.-stdin child) input)
         (.end (.-stdin child)))))))
