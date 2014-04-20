(ns quilsample.circles
  (:use quil.core))

(def cnum 10)

(defprotocol Drawer (draw [_]))
(deftype Point [x y])
(defrecord Circle [location radius line-color fill-color alpha]
  Drawer
  (draw [_]
    (let [x (.x location) y (.y location)]
    (no-stroke)
    (fill fill-color alpha)
    (ellipse x y (* radius 2) (* radius 2))
    (stroke line-color 150)
    (no-fill)
    (ellipse x y 10 10))))

(defn init-circle []
  (let [loc (Point. (random (width)) (random (height)))
        radius (+ (random 100) 10)
        l-color (color (random 255) (random 255) (random 255))
        f-color (color (random 255) (random 255) (random 255))
        alpha (random 255)]
  (Circle. loc radius l-color f-color alpha)))

(defn draw-circle [{:keys [x y radius line-clr fill-clr alpha]}]
  (no-stroke)
  (fill fill-clr alpha)
  (ellipse x y (* radius 2) (* radius 2))
  (stroke line-clr 150)
  (no-fill)
  (ellipse x y 10 10))

(defn draw-circles []
  (dotimes [_ cnum]
    (draw (init-circle))))

(defn setup []
  (background 255)
  (smooth)
  (stroke-weight 1)
  (fill 150 50)
  (draw-circles))

(defn mouse-released []
  (draw-circles))

(defsketch circles
  :title "circles"
  :size [1000 800]
  :setup setup
  :mouse-released mouse-released)

(defn -main [] ())
