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


(defn update-point [{:keys [x y oldx oldy]}]
  (let [vx (* (- x oldx) friction)
        vy (* (- y oldy) friction)]
    {:x (+ x vx) :y (+ y vy gravity) :oldx x :oldy y}))


(defn update-points [state]
  (swap! state :points (map-kv update-point (:points @state)))
  state)


(defn apply-stick-constraints [state]
  (swap! state :sticks (:sticks @state))
  state)


(defn constrain-point [{:keys [x y oldx oldy] :as point}]
   (let [vx (* (- x oldx) friction)
         vy (* (- y oldy) friction)]
    (cond
      ;; Hit the floor
      (> y height) {:x x :y height :oldx oldx :oldy (+ height (* vy bounce))}
      ;; Hit the ceiling
      (< y 0)      {:x x :y 0 :oldx oldx :oldy (* vy bounce)}
      ;; Hit the left wall
      (< x 0)      {:x 0 :y y :oldx (* vx bounce) :oldy oldy}
      ;; Hit the right wall
      (> x width) {:x width :y y :oldx (+ width (* vx bounce)) :oldy oldy}
      ;; Free movement
      :else point)))


(defn apply-world-constraints [state]
  (swap! state :points (map-kv constrain-point (:points @state)))
  state)


(defn update-state [state]
  (->> state
       (update-points)
       (apply-stick-constraints)
       (apply-world-constraints)))


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(defn setup []
  (quil/frame-rate  30)
  (quil/background 255)
  (atom {:points {:p0 {:x   3 :y   1 :oldx   0 :oldy    0}
                  :p1 {:x 100 :y 100 :oldx 100 :oldy  100}
                  :p2 {:x   0 :y 500 :oldx  -5 :oldy  524}}
         :sticks [{:links [:p0 :p1] :length "tbd: distance calculation"}]}))


(defn draw [state]
  (quil/background 255)
  (quil/fill 0)
  (doseq [point (vals (:points @state))]
    (quil/ellipse (:x point) (:y point) 7 7)))


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
