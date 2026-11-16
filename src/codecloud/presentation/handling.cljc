(ns codecloud.presentation.handling
  "What the view's handlers return. A handler takes the whole view state
  and an event's arguments and gives back {:state :effects}, the effects
  being what the renderer does next, such as [:send message] for the
  host."
  (:require [codecloud.protocol :as protocol]))

(defn unchanged
  "The outcome of `state`, changed by the handler or not, with nothing
  more to do."
  [state]
  {:state state :effects []})

(defn changed
  "The outcome of `state` with `effects` to carry out, leaving out nils."
  [state & effects]
  {:state state :effects (vec (remove nil? effects))})

(defn send-effect [message-type fields]
  [:send (protocol/message message-type fields)])

(defn with-notice
  "`state` showing `notice`, among the latest few."
  [state notice]
  (let [kept-notices 4]
    (update state :notices #(vec (take-last kept-notices (conj (vec %) notice))))))
