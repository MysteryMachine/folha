(ns folha.core
  (:use arcadia.core)
  (:require [clojure.data :refer [diff]]
            [clojure.edn :as edn])
  (:import
   [UnityEngine  Vector3
    NavMeshAgent Animator
    Debug        Physics
    Transform    RaycastHit
    Input        Ray
    Vector2      Camera
    Resources    Quaternion
    GameObject   Screen
    Rigidbody    Rect
    Mathf TextureFormat
    Texture2D    Color
    QueryTriggerInteraction
    PlayerPrefs Application
    UI.Text]
   ArcadiaState))

;; Logging
(defn log
  "* [msg]
     Shortcut for logging in Unity. Check your Unity console.
     - `msg` : Whatever it is you'd like to log."
  [msg]
  (Debug/Log (str msg)))

;; Accessing Objects
(defn the
  "* [arg]
     Used for accessing objects in the Unity graph using just their
     name. If a string is not passed in, it will just return the
     object passed in. This behavior exists in order to faciliate
     library functionality. Most fns in folha can take a string or
     a Unity object.
     - `arg` : a string or a Unity object
   * [obj component]
     Like above, but also reaches into the object to get a component.
     - `obj` : a string or a Unity object
     - `component` : name (string or type) of a component"
  ([arg] (if (string? arg) (object-named arg) arg))
  ([obj component]
   (if-let [go (the obj)]
     (get-component go component)
     nil)))

(defn the*
  "* [obj component]
     Like `the`, but searches for the component in children as well.
     - `obj` : a string or a Unity object
     - `component` : name (string or type) of a component"
  [obj component]
  (.GetComponentInChildren (the obj) component))

(defn go?
  "* [obj]
     Returns true if `obj` is a GameObject and false otherwise.
     - `obj` : Anything"
  [obj]
  (= GameObject (type obj)))

(defn ->go
  "* [obj]
     Returns the GameObject associate with `obj`
     - `obj` : A Unity component"
  [obj]
  (.gameObject obj))

(defn transform [obj] (the obj Transform))

(defn parent [obj] (.parent (transform obj)))
(defn parent? [par obj] (= (parent obj) (transform par)))
(defn parent! [obj par]
  (set! (.parent (the obj Transform)) (the par Transform)))

(defn child-components
  ([name] (child-components name Transform))
  ([name component]
   (let [obj (the name)
         prelim (.GetComponentsInChildren obj component)]
     (filter #(parent? % obj) prelim))))

(defn children
  ([top-obj] (children top-obj identity))
  ([top-obj filter-fn]
   (let [kids (map ->go (child-components top-obj Transform))]
     (filter #(and (parent? % top-obj) (filter-fn %)) kids))))

(defn ->name ^String [obj] (.name obj))

(defn active!
  "* [obj]
     Toggles the activeness of a GameObject
     - `obj` : A Unity GameObject
   * [obj v]
     Sets the activeness of a GameObject to `v`
     - `v` : a boolean value"
  ([obj]
   (.SetActive obj (not (.activeSelf obj))))
  ([obj v]
   (.SetActive obj v)))

;; Greater of and Lesser of
(defn <of [a b] (if (< a b) a b))
(defn >of [a b] (if (> a b) a b))

;; Math

(defn sqrt [n] (Mathf/Sqrt n))
(defn ceil [n] (Mathf/Ceil n))

;; Vector
(defn v2 [x y] (Vector2. x y))

(defn v2op [op v1 vs]
  (v2 (reduce #(op %1 (.x %2)) (.x v1) vs)
      (reduce #(op %1 (.y %2)) (.y v1) vs)))
(defn v2-
  ([v] (v2 (- (.x v))
           (- (.y v))))
  ([v v1 & vs] (v2op - v (cons v1 vs))))

(defn v2+ [v1 & vs] (v2op + v1 vs))

(defn v2* [v n]
  (v2 (* n (.x v)) (* n (.y v))))

(defn v3
  (^Vector3 [[x y z]] (Vector3. x y z))
  (^Vector3 [x y z]   (Vector3. x y z)))

(defn v3op [op v1 vs]
  (v3 (reduce #(op %1 (.x %2)) (.x v1) vs)
      (reduce #(op %1 (.y %2)) (.y v1) vs)
      (reduce #(op %1 (.z %2)) (.z v1) vs)))

(defn v3-
  ([v] (v3 (- (.x v))
           (- (.y v))
           (- (.z v))))
  ([v v1 & vs] (v3op - v (cons v1 vs))))

(defn v3+ [v & vs] (v3op + v vs))

(defn v3* [v n]
  (v3 (* n (.x v))
      (* n (.y v))
      (* n (.z v))))

(defn vx [v] (.x v))
(defn vy [v] (.y v))
(defn vz [v] (.z v))

(defn v2x [v i] (v2 i (vy v)))
(defn v2y [v i] (v2 (vx v) i))

(defn v3x [v i] (v3 i (vy v) (vz i)))
(defn v3y [v i] (v3 (vx v) i (vz i)))
(defn v3z [v i] (v3 (vx v) (vy v) i))

(defn v2->v3 [v]
  (v3 (.x v) (.y v) 0))
(defn v3->v2 [v]
  (v2 (.x v) (.y v)))

(defn v2->clj [v] {:x (.x v) :y (.y v)})
(defn v3->clj [v] {:x (.x v) :y (.y v) :z (.z v)})
(defn clj->v2 [v] (v2 (:x v) (:y v)))
(defn clj->v3 [v] (v3 (:x v) (:y v) (:z v)))

(defn q4
  (^Quaternion [[x y z a]] (Quaternion. x y z a))
  (^Quaternion [x y z a] (Quaternion. x y z a)))

(defn mag    ^Double [obj] (.magnitude obj))
(defn sqmag  ^Double [obj] (.sqrMagnitude obj))
(defn normal ^Double [obj] (.normalized obj))

;; Tranform

(defn forward [obj] (.forward (the obj Transform)))

(defn position ^Vector3 [obj] (.position (transform obj)))
(defn position!
  ([obj ^Vector3 v] (set! (.position (transform obj)) v))
  ([obj x y z] (set! (.position (transform obj)) (v3 x y z))))

(defn local-position ^Vector3 [obj] (.localPosition (transform obj)))
(defn local-position!
  ([obj ^Vector3 v] (set! (.localPosition (transform obj)) v))
  ([obj x y z] (set! (.localPosition (transform obj)) (v3 x y z))))

(defn rotation ^Quaternion [obj] (.rotation (transform obj)))
(defn rotation!
  ([obj q] (set! (.rotation (transform obj)) q))
  ([obj x y z a] (set! (.rotation (transform obj)) (q4 x y z a))))

(defn scale ^Vector3 [obj] (.localScale (transform obj)))
(defn scale!
  ([obj ^Vector3 v] (set! (.localScale (transform obj)) v))
  ([obj x y z] (set! (.localScale (transform obj)) (v3 x y z))))

(defn dist [a b] (Vector3/Distance (position a) (position b)))

;; Color

(defn color [r g b a] (Color. r g b a))

;; Nav Mesh Agent
(defn nav-mesh-agent ^NavMeshAgent [obj] (the obj NavMeshAgent))
(defn nav-mesh-agent* ^NavMeshAgent [obj] (the* obj NavMeshAgent))
(defn move!
  ([obj target]
   (let [coords (if (= Vector3 (type target))
                  target
                  (position target))]
     (set! (.destination (the obj NavMeshAgent)) coords)))
  ([obj x y z] (move! obj (v3 x y z))))

;; Animator
(defn animator  ^Animator [obj] (the obj Animator)) 
(defn animator* ^Animator [obj] (the* obj Animator))

;; Look into maybe using a macro to define all these
;; in the future?
(defmulti  anim-set*! #(type %3))
(defmethod anim-set*! Boolean [this ^String name arg]
  (.SetBool (animator* this) name arg))
(defmethod anim-set*! nil [this ^String name _]
  (.SetTrigger (animator* this) name))
(defmethod anim-set*! Double [this ^String name ^Double arg]
  (.SetFloat (animator* this) name (float arg)))
(defmethod anim-set*! Single [this ^String name arg]
  (.SetFloat (animator* this) name arg))
(defmethod anim-set*! Int64 [this ^String name ^Int64 arg]
  (.SetInteger (animator* this) name (int arg)))
(defmethod anim-set*! Int32 [this ^String name arg]
  (.SetInteger (animator* this) name arg))
(defmethod anim-set*! :default [this name arg]
  (throw (str "Unsure how to set animation " arg " for property " name)))

(defn sync-agent-velocity!
  "Update hook meant for syncing a velocity field in an animtor
  to a NavMeshAgent's velocity on or inside a GameObject."
  [this state]
  (anim-set*! this "velocity" (mag (.velocity (nav-mesh-agent* this))))
  state)

;; Mouse

(defn mouse-pos  ^Vector3 [] (Input/mousePosition))
(defn v2mpos []
  (let [-mpos (mouse-pos)]
    (v2 (.x -mpos) (.y -mpos))))

(defn right-click [] (Input/GetMouseButtonDown 1))
(defn right-held  [] (Input/GetMouseButton     1))
(defn right-up    [] (Input/GetMouseButtonUp   1))

(defn left-click  [] (Input/GetMouseButtonDown 0))
(defn left-held   [] (Input/GetMouseButton     0))
(defn left-up     [] (Input/GetMouseButtonUp   0))

;; Canvas

(defn text!
  "* [o s]
     Given a UnityObject, set the text of it, or its first child,
     to a certain string.
     - `o` : A UnityObject
     - `s` : A string"
  [o s]
  (let [t (the* o Text)]
    (set! (.text t) s)))

;; Screen

(defn main-camera ^Camera [] (Camera/main))
(defn scrn-dims []
  (v2 Screen/width Screen/height))
(defn scrn->worldpt [v]
  (.ScreenToWorldPoint
   (main-camera)
   (v3 (.x v)
       (.y v)
       (.nearClipPlane (main-camera)))))
(defn mouse->scrn []
  (.ScreenPointToViewportPoint (main-camera) (mouse-pos)))
;; Raycasting

(defn world->scrnpt
  ([camera pt] (.WorldToScreenPoint camera pt))
  ([pt] (world->scrnpt (main-camera) pt)))
(defn obj->scrnpt
  ([camera obj] (world->scrnpt camera (position obj)))
  ([obj] (world->scrnpt (position obj))))
(defn scrnpt->ray
  ([camera pt] (.ScreenPointToRay camera pt))
  ([pt] (.ScreenPointToRay (main-camera) pt)))

(defn mouse->ray
  ([camera] ^Ray (scrnpt->ray camera (mouse-pos)))
  ([] ^Ray (scrnpt->ray (main-camera) (mouse-pos))))

(defn ray->direction [^Ray ray] (.direction ray))

(defn raycast [^Ray ray &
               {:keys [max-distance layer-mask query-trigger-interaction]
                :or {max-distance Mathf/Infinity
                     layer-mask Physics/DefaultRaycastLayers
                     query-trigger-interaction QueryTriggerInteraction/UseGlobal}}]
  (Physics/RaycastAll ray max-distance layer-mask query-trigger-interaction))
(defn sweep [obj v] (.SweepTestAll (the obj Rigidbody) v))
(defn boxcast
  ([worldpt size] (boxcast worldpt size identity))
  ([worldpt size op]
   (let [dir (forward (main-camera))
         cube (create-primitive :cube)]
     (parent! cube (main-camera))
     (add-component cube Rigidbody)
     (set! (.useGravity (the cube Rigidbody)) false)
     (set! (.position (.transform cube)) worldpt)
     (set! (.localRotation (.transform cube)) (q4 0 0 0 1))
     (set! (.localScale (.transform cube)) size)
     (let [swept (sweep cube dir)]
       (destroy cube)
       (filter
        (fn [o]
          (if-let [trs (.transform o)]
            (if-let [go (.gameObject trs)]
              (op go))))
        (vec swept))))))

;; TODO: This function seems to be doing too much, break up in the future
(defn mouse->hit
  ([] (mouse->hit (fn [_] true) (fn [_] false)))
  ([obj-filter] (mouse->hit obj-filter (fn [_] false)))
  ([obj-filter point-filter &
    ;; Uh, figure out a way to avoid unneccesary duplication
    {:keys [max-distance layer-mask query-trigger-interaction]
     :or {max-distance Mathf/Infinity
          layer-mask Physics/DefaultRaycastLayers
          query-trigger-interaction QueryTriggerInteraction/UseGlobal}}]
   (let [hit (first (raycast (mouse->ray) :layer-mask layer-mask))]
     (when hit
       (let [go   (->go (.transform hit))]
         (cond
           (obj-filter go) go
           (point-filter go) (.point hit)
           :else nil))))))

;; Textures

(defn texture [x y & {:keys [format mipmap]
                      :or {format TextureFormat/RGBA32 mipmap false}}]
  (Texture2D. x y format mipmap))

;; Prefab
(defn clone!
  ([^GameObject obj]
   (let [go (GameObject/Instantiate obj)]
     (set! (.name go) (.name obj))))
  ([^GameObject obj ^Vector3 pos ^Quaternion rot]
   (let [go (GameObject/Instantiate obj pos rot)]
     (set! (.name go) (.name obj)))))

(defn prefab!
  ([^String name] (clone! (Resources/Load name)))
  ([^String name  ^Vector3 pos]
   (clone! (Resources/Load name) pos (q4 0 0 0 1)))
  ([^String name  ^Vector3 pos ^Quaternion rot]
   (clone! (Resources/Load name) pos rot)))

;; PlayerPrefs

(defn save!
  "* [str-fn m]
     Saves a map to PlayerPrefs.
     `str-fn` : A stringifying function. If your game only
     uses a singular way of stringifying, consider creating
     a partial function to promote reuse.
     `m` : a map or a sequence of key value pairs. Saves
      each key to its own PlayerPref, but is shallow, so
      nested maps all get saved to the same PlayerPref."
  ([str-fn m]
   (doseq [[k v] m]
     (PlayerPrefs/SetString (str-fn k) (str-fn v))))
  ([str-fn k1 v1 & {:as opts}]
   (let [kvs (assoc opts k1 v1)]
     (doseq [[k v] kvs]
       (PlayerPrefs/SetString (str-fn k) (str-fn v))))))

(defn load!
  "* [reader & ks]
     Reads data from PlayerPrefs.
     - `reader` : A reader function, usually one that calls
       clojure.edn/read-string with a map of readers. If you
       only ever use one reader, you can define a partial function
       where this reader is always included to save repitition.
     - `ks` : can either be multiple arguments in the form of
       (load! :a :b :c) or as one singular list in the form off
       (load [:a :b :c]). These represent string keys to be
       read off of player preferences
     WARNING: behavior is undefined for things that are not
     keywords and strings."
  ([reader & ks]
   (into {}
         (for [k (flatten ks)]
           [k (let [pref (PlayerPrefs/GetString (str k))]
                (if-not (empty? pref)
                  (reader pref)
                  nil))]))))

(defn delete-pref
  "* [k]
     Deletes a PlayerPref key.
     - `k` : key"
  [k]
  (PlayerPrefs/DeleteKey (str k)))

;; Load Scene
(defn quit!
  "* []
     Calls Application/Quit"
  []
  (Application/Quit))

(defn load-scene!
  "* [i]
     Loads a scene
     - `i` : an integer"
  [i]
  (Application/LoadLevel i))

;; Arcadia State
(defn state-component
  "* [obj]
     Accesses the ArcadiaState of an Object.
     - `obj` : A Unity object with an ArcadiaState component."
  [obj]
  (the obj ArcadiaState))

(defn ->state
  "* [obj]
     Accesses the ArcadiaState.state of an Object.
     - `obj` : A Unity object with an ArcadiaState component."
  [obj]
  (if-let [state-comp (state-component obj)]
    (.state state-comp)))

(defn reset!!
  "* [obj arg]
     Like `reset!` for ArcadiaState. Should throw an error
     if the object does not contain such a state.
     - `obj` : A Unity object with an ArcadiaState component.
     - `arg` : The new value of your ArcadiaState"
  [obj arg]
  (set! (.state (the obj ArcadiaState)) arg))

(defn swap!!
  "* [obj fun]
     Like `swap!` for ArcadiaState. Should throw an error
     if the object does not contain such a state.
     - `obj` : A Unity object with an ArcadiaState component.
     - `fun` : A function with one argument. Will be called with
       the current ArcadiaState state, and its return value will
       set the new ArcadiaState."
  [obj fun]
  (let [st (the obj ArcadiaState)]
    (set! (.state st) (fun (.state st)))))

(defn merge!
  "* [obj m]
     Merges in a map into the state of an object with an
     ArcadiaState.
     - `obj` : A Unity handle with an ArcadiaState component
     - `m` : The map to be merged in
     -> The merged state."
  [obj m]
  (swap!! obj #(merge % m))
  (->state obj))

(defmacro defhook
  "[name [this state] & body]
   Defines a function to be used as a hook for an object with
   an ArcadiaState component. The return value of this function
   will be the new state of the object.
   - `name` : The name of the created function
   - `this` : The symbol to which the GameObject will be bound to
   - `state`: The symbol to which the GameObject's ArcadiaState.state
     will be bound to.
   - `body` : The body of the function"
  [name [this state] & body]
  `(defn ~name [~this]
     (let [~state (->state ~this)]
       (reset!! ~this (do ~@body)))))

(defn- hook-expand [prefab decl]
  (let [hook-name (first decl)
        args (second decl)
        this (first args)
        state (second args)
        body (drop 2 decl)]
    `(let [c# (add-component ~prefab ~hook-name)]
       (set! (.fn c#)
             (fn [~this]
               (let [~state (->state ~this)]
                 (reset!! ~this (do ~@body))))))))

(defmacro +state
  "Deprecated, as of March 20, 2016, works, but is no longer
   part of the core design of folha
   * [prefab & hooks]
     Takes a prefab and a set of hooks, and adds those hooks
     to that prefab. Use with load-hooks to avoid compile time
     errors, or import the required hooks by hand.
     - `prefab` : A Unity object
     - `hooks`  : A hook consists of a hook name; argument list
       (which itself can consist of a `this` which refers to the
       Unity object and a `state`, which refers to the ArcadiaState
       on the object; and a body, whose return will be automatically
       set as the new ArcadiaState.
   Example
   (+state prefab
     (UpdateHook [this state] (sync-agent-velocity! this state))
     (UpdateHook [this state] (e/update! this state)))"
  [prefab & hooks]
  (let [sym (gensym)]
    `(let [~sym ~prefab]
       (when-not (->state ~sym)
         (add-component ~sym ArcadiaState))
       ~@(map (fn [hook] (hook-expand sym hook)) hooks))))

(defmacro load-hooks
  "Deprecated, as of March 20, 2016, works, but is no longer
   part of the core design of folha
   * []
     Loads some of the simplest hooks used in Arcadia's component library."
  []
  `(import ArcadiaState
           StartHook
           UpdateHook))

;; Caching & Functional State

(defn cached-load!
  "* [reader object & ks]
     Checks the Arcadia state of object for a set of keys.
     If the keys are not contained in the state, it reads
     data from PlayerPrefs.
     - `reader` : A reader function, usually one that calls
       clojure.edn/read-string with a map of readers. If you
       only ever use one reader, you can define a partial function
       where this reader is always included to save repitition.
     - `object` : A string or unity object with an ArcadiaState
       component attached. All data will be saved here. As with the
       reader above, use a partial if this is always the same object.
     - `ks` : can either be multiple arguments in the form of
       (cached-load! :a :b :c) or as one singular list in the form off
       (cached-load [:a :b :c]). These represent string keys to be
       read off of player preferences"
  [reader object & ks]
  (let [this (the object)
        state (->state this)
        fks (flatten (seq ks))
        state-ks-set (set (keys state))
        req-ks-set (set fks)
        [new-ks _ rest-ks] (diff req-ks-set state-ks-set)
        old-data (select-keys state rest-ks)]
    (if new-ks
      (let [new-data (load! reader (vec new-ks))]
        (merge! this new-data)
        (merge old-data new-data))
      old-data)))

(defn all-snaps
  "* []
     Returns all snapshot ids."
  []
  (or (load! edn/read-string [:folha/snapshots])
      []))

(defn snapshot!
  "* [str-fn o]
     A function meant for debugging and/or the REPL.
     Snapshots the Arcadia of an object to promote debugging.
     Only useful if your scene is set to be a function of your
     state.
     * `str-fn` : A stringifying function. If your game only
     uses a singular way of stringifying, consider creating
     a partial function to promote reuse.
     * `o` : A Unity object (or its name) with an ArcadiaState component."
  [str-fn o]
  (let [id (str "ID::" System.DateTime/Now)
        snaps (all-snaps)]
    (save! str-fn {id (->state o)})
    (save! str (vec (conj all-snaps id)))
    id))

(defn load-snap!
  "* [reader object]
     Load the last snapshot taken.
     - `reader` : A reader function, usually one that calls
       clojure.edn/read-string with a map of readers. If you
       only ever use one reader, you can define a partial function
       where this reader is always included to save repitition.
     - `object` : A string or unity object with an ArcadiaState
       component attached. All data will be saved here. As with the
       reader above, use a partial if this is always the same object.
   * [reader object id]
     Loads the snapshot with this id.
     - `reader` : see above
     - `object` : see above
     - `id` : the snapshot id"
  ([reader object]
   (load-snap! reader object (last (all-snaps))))
  ([reader object id]
   (reset!! object (load! reader [id]))))

(defn clear-snaps
  "[]
   Deletes all snapshots."
  []
  (doseq [id (all-snaps)]
   (delete-pref id)))

(defmacro generate-data-fns
  "Macro for creating partial initialized versions of read!,
   reset!, swap!, load!, and merge!. Useful if you're only using one
   edn reader, and one GameObject as a source of truth.

   read!, in this case, is actually a partial of cached-load!

   * [name reader]
     `name` : The name of the GameObject with the ArcadiaState
     `reader` : An edn reader. Can be edn/read-string, or your
     own custom reader."
  [name reader]
  `(let [name# ~name
         reader# ~reader]
     (~'def ~'load!  (partial load! reader#))
     (~'def ~'read!  (partial cached-load! reader# name#))
     (~'def ~'reset!! (partial reset!! name#))
     (~'def ~'swap!!  (partial swap!! name#))
     (~'def ~'merge! (partial merge! name#))
     (~'defn ~'this [] (->state (the name#)))))

(defn- protocol-transform [pname api-fns]
  `(defprotocol ~pname
     ~@(map #(list (first %) (second %)) api-fns)))

(defn- component-transform [name pname api-fns]
  `(defcomponent ~name []
     ~pname
     ~@api-fns))

(defmacro defapi
  "Generates a Unity component with method you define
   yourself. Arcadia's hooks are not enough for every
   single situation. This allows you to call functions
   from buttons or from the animator.

   - Example Usage
   (defapi StartMenu
    (NewGame [this]
      (save-party! (random-party))
      (curtains! :cutscene :intro))
    (LoadGame [this]
      (curtains!))
    (Exit [this]
      (quit!)))

   * [name & api-fns]
     `name` : the name of your Unity component
     `api-fns` : functions in the form of
      (FnName [this] body) where FnName is the
      method name of your function."
  [name & api-fns]
  (let [pname (symbol (str "I" name))]
   `(do
      ~(protocol-transform   pname api-fns)
      ~(component-transform  name pname api-fns))))
