(ns codecloud.model.cycles
  "Load cycles, by Tarjan's algorithm. Only edges that load their target
  count.")

(defn- open-node
  "Gives `node` the next depth-first index, pushes it on the component
  stack, and starts a frame that will walk its successors."
  [state node successors-of]
  (let [node-index (:next-index state)]
    (-> state
        (assoc-in [:index-of node] node-index)
        (assoc-in [:lowlink-of node] node-index)
        (update :next-index inc)
        (update :component-stack conj node)
        (update :on-stack conj node)
        (update :frames conj [node (seq (successors-of node))]))))

(defn- lower-lowlink [state node candidate]
  (update-in state [:lowlink-of node] min candidate))

(defn- close-component
  "Pops the component whose root is `root` off the component stack."
  [state root]
  (loop [component-stack (:component-stack state)
         on-stack (:on-stack state)
         members []]
    (let [member (peek component-stack)
          remaining-stack (pop component-stack)
          remaining-on-stack (disj on-stack member)
          collected (conj members member)]
      (if (= member root)
        (-> state
            (assoc :component-stack remaining-stack :on-stack remaining-on-stack)
            (update :components conj collected))
        (recur remaining-stack remaining-on-stack collected)))))

(defn- visit-from
  "Depth-first search from one unvisited root, driven by an explicit frame
  stack so a long dependency chain cannot overflow the call stack."
  [state root successors-of]
  (loop [state (open-node state root successors-of)]
    (if-let [[node remaining-successors] (peek (:frames state))]
      (if-let [successor (first remaining-successors)]
        (let [frames (:frames state)
              state (assoc state :frames (assoc frames (dec (count frames))
                                                [node (next remaining-successors)]))]
          (cond
            (not (contains? (:index-of state) successor))
            (recur (open-node state successor successors-of))

            (contains? (:on-stack state) successor)
            (recur (lower-lowlink state node (get-in state [:index-of successor])))

            :else
            (recur state)))
        (let [state (update state :frames pop)
              state (if-let [[parent] (peek (:frames state))]
                      (lower-lowlink state parent (get-in state [:lowlink-of node]))
                      state)]
          (recur (if (= (get-in state [:lowlink-of node]) (get-in state [:index-of node]))
                   (close-component state node)
                   state))))
      state)))

(defn strongly-connected-components
  "Strongly connected components of the graph of `node-ids` and
  `successors-of`, as vectors of ids."
  [node-ids successors-of]
  (:components
   (reduce (fn [state node]
             (if (contains? (:index-of state) node)
               state
               (visit-from state node successors-of)))
           {:next-index 0
            :index-of {}
            :lowlink-of {}
            :component-stack []
            :on-stack #{}
            :frames []
            :components []}
           node-ids)))

(defn- shortest-loop
  "A shortest path from `start` through `members` back to `start`, found
  breadth-first."
  [start members successors-of]
  (loop [frontier [[start]]
         visited #{start}]
    (when (seq frontier)
      (if-let [closing-path (some (fn [path]
                                    (when (some #(= start %) (successors-of (peek path)))
                                      path))
                                  frontier)]
        (conj closing-path start)
        (let [[next-frontier next-visited]
              (reduce (fn [[paths seen] path]
                        (reduce (fn [[inner-paths inner-seen] successor]
                                  (if (or (contains? inner-seen successor)
                                          (not (contains? members successor)))
                                    [inner-paths inner-seen]
                                    [(conj inner-paths (conj path successor))
                                     (conj inner-seen successor)]))
                                [paths seen]
                                (successors-of (peek path))))
                      [[] visited]
                      frontier)]
          (recur next-frontier next-visited))))))

(defn- cycles-within
  "Cycles formed by `edges`, all assumed to load their targets on one
  platform, as [{:members :path}] sorted by first member."
  [node-ids edges]
  (let [successors (reduce (fn [by-id {:keys [from to]}]
                             (update by-id from (fnil conj (sorted-set)) to))
                           {}
                           edges)
        successors-of (fn [node] (vec (get successors node)))
        self-requiring (into #{} (comp (filter #(= (:from %) (:to %))) (map :from)) edges)]
    (->> (strongly-connected-components (sort node-ids) successors-of)
         (filter (fn [component]
                   (or (> (count component) 1)
                       (contains? self-requiring (first component)))))
         (map (comp vec sort))
         (map (fn [members]
                {:members members
                 :path (shortest-loop (first members) (set members) successors-of)})))))

(defn find-cycles
  "{:cycles [{:id :members :path :platforms}] :cycle-id-of {id cycle-id}},
  per platform, :path being a shortest loop of requires."
  [node-ids edges]
  (let [platforms (into (sorted-set) (mapcat :load-platforms) edges)
        found (for [platform platforms
                    found-cycle (cycles-within node-ids
                                               (filter #(contains? (:load-platforms %) platform) edges))]
                (assoc found-cycle :platform platform))
        cycles (->> (group-by :members found)
                    (map (fn [[members same-cycles]]
                           {:members members
                            :path (:path (first same-cycles))
                            :platforms (into (sorted-set) (map :platform) same-cycles)}))
                    (sort-by (fn [found-cycle] [(first (:members found-cycle)) (count (:members found-cycle))]))
                    (map-indexed (fn [cycle-index found-cycle] (assoc found-cycle :id cycle-index)))
                    vec)]
    {:cycles cycles
     :cycle-id-of (reduce (fn [cycle-id-of {:keys [id members]}]
                            (reduce (fn [inner member]
                                      (if (contains? inner member) inner (assoc inner member id)))
                                    cycle-id-of
                                    members))
                          {}
                          cycles)}))

(defn cyclic-edge-predicate
  "Predicate true for an edge inside one of `cycles`, joining two members
  on a platform where that cycle exists."
  [cycles]
  (let [indexed (mapv (fn [{:keys [members platforms]}] [(set members) platforms]) cycles)]
    (fn [{:keys [from to load-platforms]}]
      (boolean
       (some (fn [[member-set platforms]]
               (and (contains? member-set from)
                    (contains? member-set to)
                    (some #(contains? load-platforms %) platforms)))
             indexed)))))
