(ns folha.core
  (:use arcadia.core)
  (:import
   [UnityEngine  Vector3
    NavMeshAgent Animator
    Debug        Physics
    Transform    RaycastHit
    Input        Ray
    Vector2      Camera
    Resources    Quaternion
    GameObject   Screen
    Rigidbody    Rect]
   ArcadiaState))

;; Logging
(defn log [msg] (Debug/Log (str msg)))

;; Accessing Object
(defn the
  "For accessing an object based on a name and component"
  ([arg] (if (string? arg) (object-named arg) arg))
  ([obj component]
   (if-let [go (the obj)]
     (get-component go component)
     nil)))

(defn the*
  [obj component]
  (.GetComponentInChildren (the obj) component))

(defn go? [obj] (= GameObject (type obj)))
(defn ->go [obj] (.gameObject obj))

(defn transform [obj] (the obj Transform))

(defn parent [obj] (.parent (transform obj)))
(defn parent? [obj par] (= (parent obj) (transform par)))
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

;; Id
(defonce ^:private ids   (atom {}))

(defn ->id [obj] (.GetInstanceID (->go (the obj))))
(defn register-id! [obj]
  (reset! ids (assoc @ids (->id obj) obj))
  obj)
(defn ->obj [id] (get @ids id))

;; Greater of and Lesser of
(defn <of [a b] (if (< a b) a b))
(defn >of [a b] (if (> a b) a b))

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

(defn scrnpt->ray  [camera pt] (.ScreenPointToRay camera pt))
(defn mouse->ray [] ^Ray (scrnpt->ray (main-camera) (mouse-pos)))

(defn ray->direction [^Ray ray] (.direction ray))

(defn raycast [^Ray ray] (Physics/RaycastAll ray))
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

(defn mouse->hit
  ([] (mouse->hit (fn [_] true) (fn [_] false)))
  ([obj-filter] (mouse->hit obj-filter (fn [_] false)))
  ([obj-filter point-filter]
   (let [hit (first (raycast (mouse->ray)))]
     (when hit
       (let [go   (->go (.transform hit))]
         (cond
           (obj-filter go) go
           (point-filter go) (.point hit)
           :else nil))))))

;; Prefab
(defn clone!
  ([^GameObject obj]
   (let [go (GameObject/Instantiate obj)]
     (set! (.name go) (.name obj))
     (register-id! go)))
  ([^GameObject obj ^Vector3 pos ^Quaternion rot]
   (let [go (GameObject/Instantiate obj pos rot)]
     (set! (.name go) (.name obj))
     (register-id! go))))

(defn prefab!
  ([^String name] (clone! (Resources/Load name)))
  ([^String name  ^Vector3 pos]
   (clone! (Resources/Load name) pos (q4 0 0 0 1)))
  ([^String name  ^Vector3 pos ^Quaternion rot]
   (clone! (Resources/Load name) pos rot)))

;; Arcadia State
(defn state-component [obj] (the obj ArcadiaState))
(defn ->state  [obj] (if-let [state-comp (state-component obj)]
                     (.state state-comp)))
(defn state! [obj arg] (set! (.state (the obj ArcadiaState)) arg))
(defn swat! [obj fun]
  (let [st (the obj ArcadiaState)]
    (set! (.state st) (fun (.state st)))))

;; MACROZ

(defn hook-expand [prefab decl]
  (let [hook-name (first decl)
        args (second decl)
        this (first args)
        state (second args)
        body (drop 2 decl)]
    `(let [c# (add-component ~prefab ~hook-name)]
       (set! (.fn c#)
             (fn [~this]
               (let [~state (->state ~this)]
                 (state! ~this (do ~@body))))))))

(defmacro +state [prefab & hooks]
  (let [sym (gensym)]
    `(let [~sym ~prefab]
       (when-not (->state ~sym)
         (add-component ~sym ArcadiaState))
       ~@(map (fn [hook] (hook-expand sym hook)) hooks))))


; Lazily only loading these for now. Add all later
; When not a on a deadline
(defmacro load-hooks []
  `(import ArcadiaState
           StartHook
           UpdateHook))

