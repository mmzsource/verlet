(ns verlet.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil-mw]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Physics Simulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def height   500)   ;; world height
(def width    500)   ;; world width
(def bounce   0.9)   ;; 10% velocity loss after hitting world border
(def gravity  0.5)   ;; world gravity
(def friction 0.995) ;; 0.5% velocity loss in every step


(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))


(defn distance-map [p0 p1]
  (let [dx (- (:x p1) (:x p0))
        dy (- (:y p1) (:y p0))
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    {:dx dx :dy dy :distance distance}))


(defn update-point [{:keys [x y oldx oldy pinned]}]
  (let [vx (* (- x oldx) friction)
        vy (* (- y oldy) friction)]
    (if pinned
      {:x x :y y :oldx oldx :oldy oldy :pinned pinned}
      {:x (+ x vx) :y (+ y vy gravity) :oldx x :oldy y})))


(defn update-points [state]
  (swap! state assoc :points (map-kv update-point (:points @state)))
  state)


(defn calc-stick-constraint [stick p0 p1]
  (let [distance-map (distance-map p0 p1)
        difference   (- (:length stick) (:distance distance-map))
        percentage   (/ (/ difference (:distance distance-map)) 2)
        offsetX      (* (:dx distance-map) percentage)
        offsetY      (* (:dy distance-map) percentage)
        p0-new       {:x      (- (:x p0) offsetX)
                      :y      (- (:y p0) offsetY)
                      :oldx   (:oldx p0)
                      :oldy   (:oldy p0)
                      :pinned (:pinned p0)}
        p1-new       {:x      (+ (:x p1) offsetX)
                      :y      (+ (:y p1) offsetY)
                      :oldx   (:oldx p1)
                      :oldy   (:oldy p1)
                      :pinned (:pinned p1)}]
    [(if (:pinned p0) p0 p0-new) (if (:pinned p1) p1 p1-new)]))


(defn apply-stick-constraint [state stick]
  (let [p0-key     (first  (:links stick))
        p0         (p0-key (:points @state))
        p1-key     (last   (:links stick))
        p1         (p1-key (:points @state))
        new-points (calc-stick-constraint stick p0 p1)]
    (swap! state assoc-in [:points p0-key] (first new-points))
    (swap! state assoc-in [:points p1-key] (last  new-points))))


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


(defn constrain-point [{:keys [x y oldx oldy pinned] :as point}]
   (let [vx (* (- x oldx) friction)
         vy (* (- y oldy) friction)]
    (cond
      ;; Hit the floor
      (> y height) {:x x :y height :oldx oldx :oldy (+ height (* vy bounce)) :pinned pinned}
      ;; Hit the ceiling
      (< y 0)      {:x x :y 0 :oldx oldx :oldy (* vy bounce) :pinned pinned}
      ;; Hit the left wall
      (< x 0)      {:x 0 :y y :oldx (* vx bounce) :oldy oldy :pinned pinned}
      ;; Hit the right wall
      (> x width) {:x width :y y :oldx (+ width (* vx bounce)) :oldy oldy :pinned pinned}
      ;; Free movement
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


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(defn setup []
  (quil/frame-rate  30)
  (quil/background 255)
  (let [points {:p0 {:x   3 :y   1 :oldx   0 :oldy   0}
                :p1 {:x 100 :y 100 :oldx 100 :oldy 100}
                :p2 {:x   0 :y 500 :oldx  -5 :oldy 524}
                :p3 {:x  25 :y 475 :oldx  -5 :oldy 525}
                :p4 {:x 250 :y 100 :oldx 250 :oldy 100 :pinned true}
                :p5 {:x 350 :y   1 :oldx 350 :oldy  1}}
        sticks [{:links  [:p0 :p1]
                 :length (:distance (distance-map (:p0 points) (:p1 points)))}
                {:links  [:p2 :p3]
                 :length (:distance (distance-map (:p2 points) (:p3 points)))}
                {:links  [:p4 :p5]
                 :length (:distance (distance-map (:p4 points) (:p5 points)))}]]
    (atom {:points points
           :sticks sticks})))


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


(quil/defsketch verlet
  :title      "verlet"
  :setup      setup
  :update     update-state
  :draw       draw
  :size       [width height]
  :middleware [quil-mw/fun-mode])


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
