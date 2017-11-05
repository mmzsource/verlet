(ns verlet.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [quil.core :as quil]
            [quil.middleware :as quil-mw]))

;;;;;;;;;;;;;;;;;;;;;;;;
;;      Helpers       ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-a-file [filename]
  (io/file (io/resource filename)))


(defn load-world [file]
  (edn/read-string (slurp file)))


(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Physics Simulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def height   500)   ;; world height
(def width    500)   ;; world width
(def bounce   0.9)   ;; 10% velocity loss after hitting world border
(def gravity  0.5)   ;; world gravity
(def friction 0.995) ;; 0.5% velocity loss in every step

(def world (load-world (load-a-file "particles.edn")))

(defrecord Point [x y oldx oldy pinned])

(defn distance-map [p0 p1]
  (let [dx (- (:x p1) (:x p0))
        dy (- (:y p1) (:y p0))
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    {:dx dx :dy dy :distance (if (= 0.0 distance) 0.0000000000001 distance)}))


(defn update-point [{:keys [x y oldx oldy pinned] :as point}]
  (let [vx (* (- x oldx) friction)
        vy (* (- y oldy) friction)]
    (if pinned
      point
      (->Point (+ x vx) (+ y vy gravity) x y pinned))))


(defn update-points [state]
  (swap! state assoc :points (map-kv update-point (:points @state)))
  state)


(defn calc-stick-constraint [stick p0 p1]
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


(defn apply-stick-constraints [state]
  (doseq [stick (:sticks @state)]
    (let [p0-key     (first  (:links stick))
          p0         (p0-key (:points @state))
          p1-key     (last   (:links stick))
          p1         (p1-key (:points @state))
          new-points (calc-stick-constraint stick p0 p1)]
      (swap! state assoc-in [:points p0-key] (first new-points))
      (swap! state assoc-in [:points p1-key] (last  new-points))))
  state)


(defn hit-floor?       [y] (> y height))
(defn hit-ceiling?     [y] (< y 0))
(defn hit-left-wall?   [x] (< x 0))
(defn hit-right-wall?  [x] (> x width))


(defn constrain-point [{:keys [x y oldx oldy pinned] :as point}]
  (let [vx (* (- x oldx) friction)
        vy (* (- y oldy) friction)]
    (cond
      (hit-floor?      y) (->Point x height oldx (+ height (* vy bounce)) pinned)
      (hit-ceiling?    y) (->Point x 0 oldx (* vy bounce) pinned)
      (hit-left-wall?  x) (->Point 0 y (* vx bounce) oldy pinned)
      (hit-right-wall? x) (->Point width y (+ width (* vx bounce)) oldy pinned)
      ;; else: free movement
      :else point)))


(defn apply-world-constraints [state]
  (swap! state assoc :points (map-kv constrain-point (:points @state)))
  state)


(defn update-state [state]
  (->> state
       (update-points)
       (apply-stick-constraints)
       (apply-world-constraints))
  state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering and user interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn setup []
  (quil/frame-rate  30)
  (quil/background 255)
  (atom world))


(defn draw [state]
  (quil/background 255)
  (quil/fill 0)
  (doseq [point (vals (:points @state))]
    (quil/ellipse (:x point) (:y point) 7 7))
  (doseq [stick (:sticks @state)]
    (let [points (:points @state)
          p0     ((first (:links stick)) points)
          p1     ((last  (:links stick)) points)]
      (quil/line (:x p0) (:y p0) (:x p1) (:y p1)))))


(defn key-pressed [state event]
  (let [k (:raw-key event)]
    (cond
      (= \c k) (reset! state (load-world (load-a-file "cloth.edn")))
      (= \p k) (reset! state (load-world (load-a-file "particles.edn")))
      (= \s k) (reset! state (load-world (load-a-file "sticks.edn")))))
  state)


(defn mouse-point [{:keys [x y p-x p-y]}]
  (->Point x y p-x p-y nil))


(defn near-mouse-press? [mouse-point point]
  (let [distance (distance-map mouse-point (val point))]
    (and (< (Math/abs (:dx distance)) 10)
         (< (Math/abs (:dy distance)) 10))))


(defn mouse-pressed [state event]
  (let [point (some #(when (near-mouse-press? (mouse-point event) %) %) (:points @state))]
    (if (nil? point)
      (swap! state assoc :dragging nil)
      (swap! state assoc :dragging (key point))))
  state)


(defn mouse-dragged [state event]
  (if (nil? (:dragging @state))
    state
    (swap! state assoc-in [:points (:dragging @state)] (mouse-point event)))
  state)


(defn mouse-released [state event]
  (swap! state assoc :dragging nil)
  state)


(defn -main []
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
