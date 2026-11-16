(ns orchard.weather
  #?(:cljs (:require-macros [orchard.weather]))
  (:require [orchard.util :as util]))

(defmacro forecast [& body] `(do ~@body))

(defn rain [x] (util/spread x))

(defn sun [x] (util/ratio x))
