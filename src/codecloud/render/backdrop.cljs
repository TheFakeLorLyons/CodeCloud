(ns codecloud.render.backdrop
  "What the cloud floats in, drawn before the cloud as one pass over the
  whole screen, each pixel looked up along its view direction. Each kind
  of backdrop is a module making its own material and keeping it up to
  date, made the first time an environment shows it."
  (:require ["three" :as three]
            [codecloud.render.plain-view :as plain-view]
            [codecloud.render.room-view :as room-view]
            [codecloud.render.sky-view :as sky-view]))

(def ^:private kinds
  "Each backdrop's module, by the name environments give it."
  {:sky {:create sky-view/create-sky :update! sky-view/update-sky!}
   :plain {:create plain-view/create-plain :update! plain-view/update-plain!}
   :room {:create room-view/create-room :update! room-view/update-room!}})

(defn create-backdrop []
  (let [^js scene (three/Scene.)
        ^js mesh (three/Mesh. (three/PlaneGeometry. 2 2))]
    (set! (.-frustumCulled mesh) false)
    (.add scene mesh)
    {:scene scene
     :camera (three/OrthographicCamera. -1 1 1 -1 0 1)
     :mesh mesh
     :made (atom {})}))

(defn- made! [{:keys [made]} kind]
  (or (get @made kind)
      (let [part ((get-in kinds [kind :create]))]
        (swap! made assoc kind part)
        part)))

(defn update-backdrop!
  "Shows the backdrop `environment` asks for, looking along `camera`, with
  this frame's time, light, and viewport in `frame-info`."
  [{:keys [^js mesh] :as backdrop} ^js camera environment frame-info]
  (let [kind (if (contains? kinds (:backdrop environment)) (:backdrop environment) :plain)
        part (made! backdrop kind)]
    (set! (.-material mesh) (:material part))
    ((get-in kinds [kind :update!]) part camera (assoc frame-info
                                                     :space (:space environment)
                                                     :gradient (:gradient environment)))))

(defn send-for!
  "Sends a visitor of `kind`, :flock or :plane, across the sky backdrop."
  [backdrop kind]
  (sky-view/send-for! (made! backdrop :sky) kind))
