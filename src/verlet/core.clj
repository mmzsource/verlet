(ns verlet.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [quil.core :as quil]
            [quil.middleware :as quil-mw]))


;;;;;;;;;;;;;;;;;;;;;;;;
;;      Helpers       ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defn load-a-file [filename]
  "Takes a filename as input and converts it to a file"
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


(def height   500)   ;; world height
(def width    500)   ;; world width
(def bounce   0.9)   ;; 10% velocity loss after hitting world border
(def gravity  0.5)   ;; world gravity
(def friction 0.995) ;; 0.5% velocity loss in every step


(defn velocity [new-val old-val]
  (* (- new-val old-val) friction))


(defrecord Point [x y oldx oldy pinned])


(def world (load-world (load-a-file "particles.edn")))


(defn distance-map
  "Calculates distances between 2 points and returns a map with dx, dy and
  distance. To prevent division by zero, returns a little more than zero when
  distance happens to be exactly zero."
  [p0 p1]
  (let [dx       (- (:x p1) (:x p0))
        dy       (- (:y p1) (:y p0))
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    {:dx dx :dy dy :distance (if (= 0.0 distance) 0.0000000000001 distance)}))


(defn update-point
  "Takes a point as input and returns the old point if the point is pinned and
  a newly constructed point otherwise. The newly constructed point will take
  the points' velocity and the specified friction and gravity into account."
  [{:keys [x y oldx oldy pinned] :as point}]
  (let [vx (velocity x oldx)
        vy (velocity y oldy)]
    (if pinned
      point
      (->Point (+ x vx) (+ y vy gravity) x y pinned))))


(defn update-points
  "Takes the world state as input, updates all the points and returns the new
  world state."
  [state]
  (swap! state update :points (partial map-kv update-point))
  state)


(defn apply-stick-constraint
  "Takes 2 points and the stick connecting them as input and calculates the new
  positions of the points taking the sticks' length into account. It calculates
  the percentage difference between the actual distance of the 2 points and the
  length of the stick. It then moves both points towards the correct length of
  their connecting stick. (But only if the point isn't 'pinned' in the world)"
  [stick p0 p1]
  (let [distance-map (distance-map p0 p1)
        difference   (- (:length stick) (:distance distance-map))
        percentage   (/ (/ difference (:distance distance-map)) 2)
        offsetX      (* (:dx distance-map) percentage)
        offsetY      (* (:dy distance-map) percentage)
        p0-new       (->Point
                      (- (:x p0) offsetX)
                      (- (:y p0) offsetY)
                      (:oldx p0)
                      (:oldy p0)
                      (:pinned p0))
        p1-new       (->Point
                      (+ (:x p1) offsetX)
                      (+ (:y p1) offsetY)
                      (:oldx p1)
                      (:oldy p1)
                      (:pinned p1))]
    [(if (:pinned p0) p0 p0-new) (if (:pinned p1) p1 p1-new)]))


(defn apply-stick-constraints
  "Takes the world state as input, applies stick constraints and returns the
  new world state."
  [state]
  (doseq [stick      (:sticks @state)]
    (let [p0-key     (first  (:links stick))
          p0         (p0-key (:points @state))
          p1-key     (last   (:links stick))
          p1         (p1-key (:points @state))
          new-points (apply-stick-constraint stick p0 p1)]
      (swap! state assoc-in [:points p0-key] (first new-points))
      (swap! state assoc-in [:points p1-key] (last  new-points))))
  state)


(defn hit-floor?       [y] (> y height))
(defn hit-ceiling?     [y] (< y 0))
(defn hit-left-wall?   [x] (< x 0))
(defn hit-right-wall?  [x] (> x width))


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
  (swap! state update :points (partial map-kv apply-world-constraint))
  state)


(defn update-state
  "Takes the world state as input, updates points, applies stick contraints,
  applies world constraints and returns the new world state."
  [state]
  (->> state
       (update-points)
       (apply-stick-constraints)
       (apply-world-constraints))
  state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering and user interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn setup
  "Initialize the quil framework."
  []
  (quil/frame-rate  30)
  (quil/background 255)
  (atom world))


(defn draw [state]
  "Takes the world state as input and draws all points and sticks in it."
  (quil/background 255)
  (quil/fill 0)
  (doseq [point (vals (:points @state))]
    (quil/ellipse (:x point) (:y point) 7 7))
  (doseq [stick (:sticks @state)]
    (let [points (:points @state)
          p0     ((first (:links stick)) points)
          p1     ((last  (:links stick)) points)]
      (quil/line (:x p0) (:y p0) (:x p1) (:y p1)))))


(defn key-pressed
  "Handle key-press event used to load different types of worlds."
  [state event]
  (let [k (:raw-key event)]
    (cond
      (= \c k) (reset! state (load-world (load-a-file "cloth.edn")))
      (= \p k) (reset! state (load-world (load-a-file "particles.edn")))
      (= \s k) (reset! state (load-world (load-a-file "sticks.edn")))))
  state)


(defn mouse-point
  "helper function to transform mouse events into a mouse-point in order to
  reuse point functions."
  [{:keys [x y p-x p-y]}]
  (->Point x y p-x p-y nil))


(defn near-mouse-press?
  "Determine if the user clicked near a point in the world."
  [mouse-point point]
  (let [distance (distance-map mouse-point (val point))]
    (and (< (Math/abs (:dx distance)) 10)
         (< (Math/abs (:dy distance)) 10))))


(defn mouse-pressed
  "On mouse-pressed event determine is mouse was pressed near a point in the
  world an if that's the case, remember which point the user is now dragging."
  [state event]
  (let [point (some #(when (near-mouse-press? (mouse-point event) %) %) (:points @state))]
    (if (nil? point)
      (swap! state assoc :dragging nil)
      (swap! state assoc :dragging (key point))))
  state)


(defn mouse-dragged
  "On mouse-dragged event, change the position of the dragged point to the last
  coordinate of the mouse."
  [state event]
  (if (nil? (:dragging @state))
    state
    (swap! state assoc-in [:points (:dragging @state)] (mouse-point event)))
  state)


(defn mouse-released
  "On mouse-released event, stop dragging."
  [state event]
  (swap! state assoc :dragging nil)
  state)


(defn -main
  "Setup quil in functional mode which basically means quil will pass the value
  coming out of the setup function to every other function as the first
  argument. For UI events (mouse etc), it will pass the UI event as a second
  argument."
  []
  (quil/sketch
    :host           -main
    :title          "verlet"
    :size           [width height]
    :setup          setup
    :update         update-state
    :draw           draw
    :key-pressed    key-pressed
    :mouse-pressed  mouse-pressed
    :mouse-dragged  mouse-dragged
    :mouse-released mouse-released
    :middleware     [quil-mw/fun-mode]))
