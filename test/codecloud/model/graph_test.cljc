(ns codecloud.model.graph-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.cycles :as cycles]
            [codecloud.model.graph :as graph]
            [codecloud.model.importance :as importance]
            [codecloud.model.parse :as parse]
            [codecloud.model.restriction :as restriction]
            [codecloud.model.snapshot :as snapshot]))

(def sources
  {"src/shop/core.clj" "(ns shop.core (:require [shop.cart :as cart] [shop.db :as db]))
                        (defn run [] (cart/total (db/load-cart)) (cart/total []))"
   "src/shop/cart.clj" "(ns shop.cart (:require [shop.pricing :as pricing]))
                        (defn total [items] (reduce + (map pricing/price items)))
                        (defn empty-cart [] [])"
   "src/shop/pricing.clj" "(ns shop.pricing (:require [shop.cart :as cart]))
                           (defn price [item] (if (cart/empty-cart) 0 (:price item)))"
   "src/shop/db.clj" "(ns shop.db (:require [shop.schema :as-alias schema]))
                      (defn load-cart [] [{::schema/id 1}])"
   "src/shop/schema.clj" "(ns shop.schema)"
   "src/shop/db.cljs" "(ns shop.db) (defn browser-only [] nil)"
   "src/shop/loop.clj" "(ns shop.loop (:require [shop.loop]))"
   "src/shop/sneaky.clj" "(ns shop.sneaky) (defn peek-total [] (shop.cart/total []))"
   "build.clj" "(require '[shop.core :as core]) (core/run)"})

(def file-facts
  (mapv (fn [[path text]] (parse/analyze-file path text)) sources))

(defn- edge-between [edges from to]
  (some #(when (and (= from (:from %)) (= to (:to %))) %) edges))

(deftest builds-nodes-and-edges
  (let [{:keys [nodes edges]} (graph/build-graph file-facts)]
    (testing "files of one namespace merge into one node"
      (is (= ["src/shop/db.clj" "src/shop/db.cljs"] (map :path (:files (get nodes "shop.db")))))
      (is (= #{"load-cart" "browser-only"} (set (map :name (:definitions (get nodes "shop.db")))))))
    (testing "a file without a namespace becomes a script node named by its path"
      (is (= :script (:kind (get nodes "build.clj")))))
    (testing "edge kinds"
      (is (= :load (:kind (edge-between edges "shop.core" "shop.cart"))))
      (is (= :alias (:kind (edge-between edges "shop.db" "shop.schema"))))
      (is (= :implicit (:kind (edge-between edges "shop.sneaky" "shop.cart"))))
      (is (= :load (:kind (edge-between edges "shop.loop" "shop.loop")))))
    (testing "reference counts"
      (is (= 2 (:reference-count (edge-between edges "shop.core" "shop.cart"))))
      (is (= 1 (:reference-count (edge-between edges "build.clj" "shop.core")))))
    (testing "inbound references add one per requiring namespace to the uses"
      (let [cart (get nodes "shop.cart")]
        (is (= ["shop.core" "shop.pricing" "shop.sneaky"] (:required-by cart)))
        (is (= (+ 2 1 1 2) (:inbound-references cart)))
        (is (= 3 (:inbound-count (some #(when (= "total" (:name %)) %) (:definitions cart)))))))
    (testing "libraries outside the codebase are listed, not linked"
      (is (= [] (:external-requires (get nodes "shop.cart")))))))

(deftest refer-all-resolves-against-definitions
  (let [facts (mapv (fn [[path text]] (parse/analyze-file path text))
                    {"src/a.clj" "(ns a (:use b)) (defn x [] (helper) (helper) (map inc []))"
                     "src/b.clj" "(ns b) (defn helper [] 1)"})
        {:keys [edges nodes]} (graph/build-graph facts)]
    (is (= 2 (:reference-count (edge-between edges "a" "b"))))
    (is (= 2 (:inbound-count (first (:definitions (get nodes "b"))))))))

(deftest finds-cycles
  (let [{:keys [nodes edges]} (graph/build-graph file-facts)
        {found :cycles cycle-id-of :cycle-id-of} (cycles/find-cycles (keys nodes) edges)]
    (is (= [["shop.cart" "shop.pricing"] ["shop.loop"]] (map :members found)))
    (is (= ["shop.cart" "shop.pricing" "shop.cart"] (:path (first found))))
    (is (= ["shop.loop" "shop.loop"] (:path (second found))))
    (is (nil? (cycle-id-of "shop.core")))
    (testing "alias-only requirements cannot close a cycle"
      (is (empty? (:cycles (cycles/find-cycles ["x" "y"] [{:from "x" :to "y" :kind :load :load-platforms #{:clj}}
                                                           {:from "y" :to "x" :kind :alias :load-platforms #{}}])))))
    (testing "a loop exists only when one platform's requires close it"
      (is (empty? (:cycles (cycles/find-cycles ["x" "y"] [{:from "x" :to "y" :kind :load :load-platforms #{:clj}}
                                                           {:from "y" :to "x" :kind :load :load-platforms #{:cljs}}])))))))

(deftest cljc-namespaces-may-require-their-own-macros
  (let [facts [(parse/analyze-file "src/lib/core.cljc"
                                   "(ns lib.core #?(:cljs (:require-macros [lib.core])) (:require [lib.util :as util]))
                                    (defmacro m [] 1) (defn f [] (util/g))")
               (parse/analyze-file "src/lib/util.cljc" "(ns lib.util) (defn g [] 2)")]
        {:keys [nodes edges]} (graph/build-graph facts)]
    (is (nil? (edge-between edges "lib.core" "lib.core")))
    (is (empty? (:cycles (cycles/find-cycles (keys nodes) edges))))
    (is (= ["lib.util"] (:requires (get nodes "lib.core"))))
    (is (= #{:clj :cljs} (:load-platforms (edge-between edges "lib.core" "lib.util"))))))

(deftest tarjan-handles-long-chains-and-nested-loops
  (let [chain-length 5000
        chain (mapv #(str "n" %) (range chain-length))
        successors (into {} (map vector chain (map vector (rest chain))))]
    (is (= chain-length (count (cycles/strongly-connected-components chain #(get successors % []))))))
  (let [components (cycles/strongly-connected-components
                    ["a" "b" "c" "d" "e"]
                    {"a" ["b"] "b" ["c"] "c" ["a" "d"] "d" ["e"] "e" ["d"]})]
    (is (= #{#{"a" "b" "c"} #{"d" "e"}} (set (map set components))))))

(deftest importance-ranks-the-busiest-namespace-first
  (let [scores (importance/scores {"big" {:functions 40 :references 30 :lines 900}
                                   "small" {:functions 1 :references 0 :lines 10}
                                   "empty" {:functions 0 :references 0 :lines 0}}
                                  importance/default-weights)]
    (is (= 1.0 (double (get scores "big"))))
    (is (< 0 (get scores "small") 1))
    (is (zero? (get scores "empty"))))
  (testing "weights shift the ranking"
    (let [scores (importance/scores {"long" {:functions 1 :references 0 :lines 5000}
                                     "used" {:functions 1 :references 80 :lines 20}}
                                    {:functions 0 :references 1 :lines 0})]
      (is (= 1.0 (double (get scores "used"))))
      (is (zero? (get scores "long")))))
  (testing "all weights zero gives every namespace zero"
    (is (= {"only" 0} (importance/scores {"only" {:functions 3 :references 2 :lines 9}}
                                         {:functions 0 :references 0 :lines 0})))))

(deftest snapshots-mark-cyclic-edges
  (let [built (snapshot/build-snapshot file-facts {:id :working-tree :label "Working tree" :time 0})]
    (is (true? (:cyclic? (edge-between (:edges built) "shop.cart" "shop.pricing"))))
    (is (false? (:cyclic? (edge-between (:edges built) "shop.core" "shop.cart"))))
    (is (= 0 (get-in built [:namespaces "shop.cart" :cycle-id])))
    (is (= 2 (get-in built [:totals :cycles])))
    (is (= 1.0 (double (reduce max (map :importance (vals (:namespaces built)))))))
    (is (= ["src/shop/db.clj" "src/shop/db.cljs"]
           (map :path (get-in built [:namespaces "shop.db" :files]))))))

(deftest hidden-pages-leave-the-measurements
  (let [built (snapshot/build-snapshot file-facts {:id :working-tree :label "Working tree" :time 0})
        hidden #{"shop.core" "build.clj"}
        shown-ids (into #{} (remove hidden) (keys (:namespaces built)))
        restricted (restriction/restrict built shown-ids)
        cart (get-in restricted [:namespaces "shop.cart"])]
    (is (identical? built (restriction/restrict built (set (keys (:namespaces built)))))
        "with nothing hidden the snapshot stays as it was")
    (is (= shown-ids (set (keys (:namespaces restricted)))))
    (testing "uses and dependents count only shown pages"
      (is (= ["shop.pricing" "shop.sneaky"] (:required-by cart)))
      (is (= 1 (:hidden-required-by cart)))
      (is (= (+ 1 1 1) (get-in cart [:metrics :references])))
      (is (= 1 (:inbound-count (some #(when (= "total" (:name %)) %) (:definitions cart))))))
    (testing "the shown pages measure as a codebase of only their files would"
      (let [alone (snapshot/build-snapshot (remove #(contains? #{"src/shop/core.clj" "build.clj"} (:path %)) file-facts)
                                           {:id :working-tree :label "Working tree" :time 0})
            measured (fn [found-snapshot]
                       (into {}
                             (map (fn [[id node]]
                                    [id [(select-keys (:metrics node) [:references :dependents :dependencies])
                                         (double (:importance node))
                                         (mapv (juxt :name :inbound-count) (:definitions node))]]))
                             (:namespaces found-snapshot)))]
        (is (= (measured alone) (measured restricted)))))
    (testing "cycles stay while a member shows, and the totals count what shows"
      (is (= 2 (get-in restricted [:totals :cycles])))
      (is (= 0 (get-in restricted [:totals :scripts])))
      (is (= (count shown-ids) (+ (get-in restricted [:totals :namespaces]) (get-in restricted [:totals :scripts]))))
      (is (every? (fn [{:keys [from to]}] (and (shown-ids from) (shown-ids to))) (:edges restricted))))))
