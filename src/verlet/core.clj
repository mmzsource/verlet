(ns verlet.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [quil.core :as quil]
            [quil.middleware :as quil-mw]))


;;;;;;;;;;;;;;;;;;;;;;;;
;;      Helpers       ;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; Compiler warnings to help speed up the simulation
;; (run `lein compile :all` to check for those warnings)
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn load-a-file
  "Takes a filename as input and converts it to a file"
  [filename]
  (io/file (io/resource filename)))


(defn load-world
  "Takes a file as input and converts it to a clojure datastructure"
  [file]
  (edn/read-string (slurp file)))


(defn map-kv
  "Takes a function and an associative collection as input and returns the
  result of applying the function to the value of each key."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Physics Simulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:const height   550)   ;; world height
(def ^:const width    500)   ;; world width
(def ^:const bounce   0.9)   ;; 10% velocity loss after hitting world border
(def ^:const gravity  0.5)   ;; world gravity
(def ^:const friction 0.995) ;; 0.5% velocity loss in every step


(defn velocity
  "Calculates the velocity and applies a friction percentage."
  ^double [^double new-val ^double old-val]
  (* (- new-val old-val) friction))


;; A record containing the current x and y position and the x and y position in
;; the previous world state. 'Pinned' is a boolean indicating if a point is
;; pinned in space. A pinned point stays on the same coordinate. You can unpin
;; a point by clicking and moving it with the mouse.
(defrecord Point [x y oldx oldy pinned])


(defn distance-map
  "Calculates distances between 2 points and returns a map with dx, dy and
  distance. To prevent division by zero, returns a little more than zero when
  distance happens to be exactly zero."
  [p0 p1]
  (let [dx       (- ^double (:x p1) ^double (:x p0))
        dy       (- ^double (:y p1) ^double (:y p0))
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    {:dx dx :dy dy :distance (if (= 0.0 distance) 0.0000000000001 distance)}))


(defn update-point
  "Takes a point as input and returns the old point if the point is pinned.
  Returns a newly constructed point otherwise. The newly constructed point will
  take the points' velocity and the specified friction and gravity into account."
  [{:keys [^double x ^double y oldx oldy pinned] :as point}]
  (if pinned
    point
    (let [vx (velocity x oldx)
          vy (velocity y oldy)]
      (->Point (+ x vx) (+ y vy gravity) x y pinned))))


(defn update-points
  "Takes the world state as input, updates all the points and returns the new
  world state."
  [state]
  (update state :points (partial map-kv update-point)))


(defn apply-stick-constraint
  "Takes 2 points and the stick connecting them as input and calculates the new
  positions of the points taking the length of the stick into account. It
  calculates the percentage difference between the actual distance of the 2
  points and the length of the stick. It then moves both points towards the
  correct length of their connecting stick."
  [stick p0 p1]
  (let [distance-map (distance-map p0 p1)
        difference   (- ^double (:length stick) ^double (:distance distance-map))
        percentage   (/ ^double (/ difference ^double (:distance distance-map)) 2)
        offsetX      (* ^double (:dx distance-map) percentage)
        offsetY      (* ^double (:dy distance-map) percentage)
        p0-new       (->Point
                      (- ^double (:x p0) offsetX)
                      (- ^double (:y p0) offsetY)
                      (:oldx p0)
                      (:oldy p0)
                      (:pinned p0))
        p1-new       (->Point
                      (+ ^double (:x p1) offsetX)
                      (+ ^double (:y p1) offsetY)
                      (:oldx p1)
                      (:oldy p1)
                      (:pinned p1))]
    [(if (:pinned p0) p0 p0-new) (if (:pinned p1) p1 p1-new)]))


(defn apply-stick-constraints
  "Takes the world state as input, applies stick constraints and returns the new
  world state."
  [state]
  (reduce
   (fn [acc stick]
     (let [p0-key     (first  (:links stick))
           p0         (p0-key (:points acc))
           p1-key     (last   (:links stick))
           p1         (p1-key (:points acc))
           new-points (apply-stick-constraint stick p0 p1)]
       (-> acc
           (assoc-in [:points p0-key] (first new-points))
           (assoc-in [:points p1-key] (last  new-points)))))
   state
   (:sticks state)))


(defn hit-floor?       [^double y] (> y height))
(defn hit-ceiling?     [^double y] (< y 0))
(defn hit-left-wall?   [^double x] (< x 0))
(defn hit-right-wall?  [^double x] (> x width))


(defn apply-world-constraint
  "Takes the world state as input, applies world constraints to all points. If
  a points hits a wall, the ceiling, or the floor, a 'bounce' velocity loss is
  calculated."
  [{:keys [x y oldx oldy pinned] :as point}]
  (let [vx (velocity x oldx)
        vy (velocity y oldy)]
    (cond
      (hit-floor?      y) (->Point x height oldx (+ height (* vy bounce)) pinned)
      (hit-ceiling?    y) (->Point x 0 oldx (* vy bounce) pinned)
      (hit-left-wall?  x) (->Point 0 y (* vx bounce) oldy pinned)
      (hit-right-wall? x) (->Point width y (+ width (* vx bounce)) oldy pinned)
      ;; else: free movement
      :else point)))


(defn apply-world-constraints
  "Takes the world state as input, applies world constraints and returns the
  new world state"
  [state]
  (update state :points (partial map-kv apply-world-constraint)))


(defn update-state
  "Takes the world state as input, updates points, applies stick contraints,
  applies world constraints and returns the new world state."
  [state]
  (->> state
       (update-points)
       (apply-stick-constraints)
       (apply-world-constraints)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering and user interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def info-message
  (str
   "\nVERLET INTEGRATION EXPERIMENT IN CLOJURE\n"
   "\nKeybindings:\n"
   "\n  * Press c to (re)start cloth simulation"
   "\n  * Press p to (re)start particle simulation"
   "\n  * Press s to (re)start sticks simulation\n"
   "\n  * Press i to see this info screen again"
   "\n  * Press q to quit\n"
   "\nMouse interactions:\n"
   "\n  * Click and drag any point"
   "\n  * Unpin a pinned point\n"))


(defn show-info-message
  "Indicate in the state that the info-message should be shown on screen"
  [state]
  (assoc state :info-message true))


(defn setup
  "Initialize the quil framework."
  []
  (quil/frame-rate 25)
  (quil/fill 0)
  (show-info-message (load-world (load-a-file "particles.edn"))))


(defn draw
  "Takes the world state as input, clears the canvas and draws all points and
  sticks found in the world state. Shows the info-message at startup and when
  requested by the user."
  [state]
  (quil/background 255)
  (if (:info-message state)
    (quil/text info-message 20 20)
    (do
      (doseq [point (vals (:points state))]
        (quil/ellipse (:x point) (:y point) 7 7))
      (doseq [stick (:sticks state)]
        (let [points (:points state)
              p0     ((first (:links stick)) points)
              p1     ((last  (:links stick)) points)]
          (quil/line (:x p0) (:y p0) (:x p1) (:y p1)))))))


(defn key-pressed
  "Handle key-press events, e.g. to load different types of worlds."
  [state event]
  (let [raw-key   (:raw-key event)
        new-state (cond
                   (= \c raw-key) (load-world (load-a-file "cloth.edn"))
                   (= \p raw-key) (load-world (load-a-file "particles.edn"))
                   (= \s raw-key) (load-world (load-a-file "sticks.edn"))
                   (= \i raw-key) (show-info-message state)
                   (= \q raw-key) (quil/exit)
                   :else state)]
    new-state))


(defn mouse-point
  "Helper function to transform mouse events into a mouse-point in order to
  reuse point functions."
  [{:keys [x y]}]
  (->Point x y x y nil))


(defn near-mouse-press?
  "Determine if the user clicked near a point in the world."
  [mouse-point point]
  (let [distance (distance-map mouse-point (val point))]
    (and (< (Math/abs ^double (:dx distance)) 10)
         (< (Math/abs ^double (:dy distance)) 10))))


(defn mouse-pressed
  "On mouse-pressed event determine is mouse was pressed near a point in the
  world an if that's the case, remember which point the user is now dragging."
  [state event]
  (let [point (some #(when (near-mouse-press? (mouse-point event) %) %) (:points state))]
    (if (nil? point)
      (assoc state :dragging nil)
      (assoc state :dragging (key point)))))


(defn mouse-dragged
  "On mouse-dragged event, change the position of the dragged point to the last
  coordinate of the mouse."
  [state event]
  (if (nil? (:dragging state))
    state
    (assoc-in state [:points (:dragging state)] (mouse-point event))))


(defn mouse-released
  "On mouse-released event, stop dragging."
  [state event]
  (assoc state :dragging nil))


(defn -main
  "Setup quil in functional mode which basically means quil will pass the value
  coming out of the setup function to every other function as the first
  argument. For UI events (mouse etc), it will pass the UI event as a second
  argument."
  []
  (quil/sketch
    :host           -main
    :title          "Verlet Integration"
    :size           [width height]
    :setup          setup
    :update         update-state
    :draw           draw
    :key-pressed    key-pressed
    :mouse-pressed  mouse-pressed
    :mouse-dragged  mouse-dragged
    :mouse-released mouse-released
    :features       [:exit-on-close]
    :middleware     [quil-mw/fun-mode]))
