(ns codecloud.render.lru
  "Caches that keep their most recently used entries.")

(defn create []
  (atom {:entries {} :clock 0}))

(defn- touched
  "The cache with `cache-key` marked as used just now."
  [{:keys [clock] :as cache-state} cache-key]
  (-> cache-state
      (assoc-in [:entries cache-key :used] clock)
      (assoc :clock (inc clock))))

(defn recall
  "The value under `cache-key`, marked as just used, or nil."
  [cache cache-key]
  (when-let [entry (get-in @cache [:entries cache-key])]
    (swap! cache touched cache-key)
    (:value entry)))

(defn remember!
  "Stores `value` under `cache-key` and returns it, dropping the least
  recently used entry once more than `capacity` are held."
  [cache cache-key value capacity]
  (swap! cache (fn [cache-state]
                 (let [stored (touched (assoc-in cache-state [:entries cache-key :value] value) cache-key)
                       entries (:entries stored)]
                   (if (> (count entries) capacity)
                     (update stored :entries dissoc (key (apply min-key (comp :used val) entries)))
                     stored))))
  value)
