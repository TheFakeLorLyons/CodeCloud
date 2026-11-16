(ns codecloud.io.ports
  "Whether a server on this computer answers yet, so a window on its
  address can wait for a server that is still starting."
  (:require ["net" :as net]
            [clojure.string :as string]))

(defn local-port
  "[host port] of web address `address` when it names this computer, or
  nil when it names another or no address at all."
  [address]
  (when-let [^js parsed (try (js/URL. address) (catch :default _error nil))]
    (let [host (string/replace (.-hostname parsed) #"^\[|\]$" "")
          default-ports {"http:" 80 "https:" 443}]
      (when (contains? #{"localhost" "127.0.0.1" "::1" "0.0.0.0"} host)
        [host (if (seq (.-port parsed))
                (js/parseInt (.-port parsed) 10)
                (get default-ports (.-protocol parsed) 80))]))))

(defn- connects?
  "Resolves to true once a connection to `address` and `port` opens, and to
  false when it is refused or a second passes first."
  [address port]
  (js/Promise.
   (fn [resolve]
     (let [patience-milliseconds 1000
           ^js socket (.createConnection net #js {:host address :port port})
           settle! (fn [connected?]
                     (.destroy socket)
                     (resolve connected?))]
       (.setTimeout socket patience-milliseconds)
       (.once socket "connect" #(settle! true))
       (.once socket "error" #(settle! false))
       (.once socket "timeout" #(settle! false))))))

(defn answers?
  "Resolves to true when something accepts connections on `port` of
  `host`, a name for this computer. localhost is tried at both its
  addresses, since a server may listen at either."
  [host port]
  (let [addresses (case host
                    "localhost" ["127.0.0.1" "::1"]
                    "0.0.0.0" ["127.0.0.1"]
                    [host])]
    (.then (js/Promise.all (into-array (map #(connects? % port) addresses)))
           (fn [results] (boolean (some true? results))))))
