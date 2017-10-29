(ns verlet.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil-mw]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Physics Simulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def height   500)
(def width    500)
(def bounce   0.9)
(def gravity  0.5)
(def friction 0.995)

(defn update-point [{:keys [x y oldx oldy]}]
  (let [vx    (* (- x oldx) friction)
        vy    (* (- y oldy) friction)
        point {:x (+ x vx) :y (+ y vy gravity) :oldx x :oldy y}]
    (cond
      ;; Hit the floor
      (> y height) {:x (:x point) :y height :oldx (:oldx point) :oldy (+ height (* vy bounce))}
      ;; Hit the ceiling
      (< y 0) {:x (:x point) :y 0 :oldx (:oldx point) :oldy (* vy bounce)}
      ;; Hit the left wall
      (< x 0) {:x 0 :y (:y point) :oldx (* vx bounce) :oldy (:oldy point)}
      ;; Hit the right wall
      (> x width) {:x width :y (:y point) :oldx (+ width (* vx bounce)) :oldy (:oldy point)}
      ;; Free movement
      :else point)))


(defn update-points [points]
  (let [new-points (map update-point @points)]
    (reset! points new-points)
    points))


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(defn setup []
  (quil/frame-rate  30)
  (quil/background 255)
  (atom [{:x 3 :y 1 :oldx 0 :oldy 0}]))


(defn draw [points]
  (quil/background 255)
  (quil/fill 0)
  (quil/ellipse (:x (first @points)) (:y  (first @points)) 7 7))


(quil/defsketch verlet
  :title      "verlet"
  :setup      setup
  :update     update-points
  :draw       draw
  :size       [width height]
  :middleware [quil-mw/fun-mode])


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
