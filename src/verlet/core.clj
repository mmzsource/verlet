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
  distance."
  [{^double p0x :x ^double p0y :y :as p0} {^double p1x :x ^double p1y :y :as p1}]
  (let [dx       (- p1x p0x)
        dy       (- p1y p0y)
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    {:dx dx :dy dy :distance distance}))


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
  [{^double length :length :as stick}
   {^double p0x :x ^double p0y :y oldp0x :oldx oldp0y :oldy pinp0 :pinned :as p0}
   {^double p1x :x ^double p1y :y oldp1x :oldx oldp1y :oldy pinp1 :pinned :as p1}]
  (let [{:keys [^double dx ^double dy ^double distance]} (distance-map p0 p1)
        difference (- length distance)
        percentage (/ (/ difference distance) 2)
        offsetX    (* dx percentage)
        offsetY    (* dy percentage)
        p0-new     (->Point (- p0x offsetX) (- p0y offsetY) oldp0x oldp0y pinp0)
        p1-new     (->Point (+ p1x offsetX) (+ p1y offsetY) oldp1x oldp1y pinp1)]
    [(if pinp0 p0 p0-new) (if pinp1 p1 p1-new)]))


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
  "Takes a point as input and applies world constraints to that point.
  Imagine a point moved from A to B in the last point update. This means a
  point record is persisted with its 'x' and 'y' being the coordinates of B
  and 'oldx' and 'oldy' being the coordinates of A.
  If a wall line crossed the imaginary line A-B, the point history should be
  rewritten. In essence, line A-B is mirrored in the wall it hits, giving rise
  to another imaginary line C-D where C mirrors A and D mirrors B.
  The simulation will take a little velocity loss into account because of the
  bounce. Therefore, a point D' is calculated on the imaginary line C-D, using
  the x and y velocities multiplied by a bounce factor.
  In the next point update (now containing C-D' coordinates instead of A-B
  coordinates), the point will fly off in exactly the right direction."
  [{:keys [^double x ^double y ^double oldx ^double oldy pinned] :as point}]
  (let [vxb (* (velocity x oldx) bounce)
        vyb (* (velocity y oldy) bounce)]
    (cond
      (hit-floor?      y) (let [miry (+ height height (- oldy))]
                             (->Point  (+ oldx vxb) (- miry vyb) oldx miry pinned))
      (hit-ceiling?    y) (let [miry (- oldy)]
                             (->Point (+ oldx vxb) (+ miry (- vyb)) oldx miry pinned))
      (hit-left-wall?  x) (let [mirx (- oldx)]
                             (->Point (+ mirx (- vxb)) (+ oldy vyb) mirx oldy pinned))
      (hit-right-wall? x) (let [mirx (+ width width (- oldx))]
                             (->Point (- mirx vxb) (+ oldy vyb) mirx oldy pinned))
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
   "\n  * Press p to (re)start points simulation"
   "\n  * Press s to (re)start sticks simulation\n"
   "\n  * Press i to see this info screen again"
   "\n  * Press q to quit\n"
   "\nMouse interactions:\n"
   "\n  * Click and drag any point"
   "\n  * Unpin a pinned point\n"))


(defn show-info-message
  "Indicate in the state that the info-message should be shown on screen"
  []
  {:info-message true})


(defn setup
  "Initialize the quil framework."
  []
  (quil/frame-rate 25)
  (quil/fill 0)
  (show-info-message))


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
                   (= \p raw-key) (load-world (load-a-file "points.edn"))
                   (= \s raw-key) (load-world (load-a-file "sticks.edn"))
                   (= \i raw-key) (show-info-message)
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
