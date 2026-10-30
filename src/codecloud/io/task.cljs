(ns codecloud.io.task
  "Promise helpers that let asynchronous IO read as a pipeline of plain
  functions.")

(defn resolved [value]
  (js/Promise.resolve value))

(defn rejected [error]
  (js/Promise.reject error))

(defn then
  "Runs `step` on the promise's value; `step` may return a value or a
  promise."
  [promise step]
  (.then promise step))

(defn recover
  "Runs `handler` on the promise's error and continues with its result."
  [promise handler]
  (.catch promise handler))

(defn all
  "Promise of a vector holding the results of every promise, in order."
  [promises]
  (.then (js/Promise.all (into-array promises)) vec))

(defn map-limited
  "Applies `start-task` to every item with at most `limit` in flight,
  resolving to the results in item order."
  [limit start-task items]
  (let [pending-items (vec items)
        item-count (count pending-items)
        results (atom (vec (repeat item-count nil)))
        next-index (atom 0)]
    (letfn [(work-through-queue []
              (let [index @next-index]
                (if (>= index item-count)
                  (resolved nil)
                  (do (swap! next-index inc)
                      (then (start-task (nth pending-items index))
                            (fn [result]
                              (swap! results assoc index result)
                              (work-through-queue)))))))]
      (then (all (doall (repeatedly (min limit item-count) work-through-queue)))
            (fn [_] @results)))))

(defn pause
  "Promise that resolves after `milliseconds`, letting other work run."
  [milliseconds]
  (js/Promise. (fn [resolve _reject] (js/setTimeout resolve milliseconds))))
