(ns quilsamples.geometry.lines
  (:use quil.core
        quilsamples.geometry))

(defn line-segment [lseg]
  (when (not (nil? lseg))
  (line (:x1 lseg) (:y1 lseg) (:x2 lseg) (:y2 lseg))))

(def click
  (let [points (atom []) ]
    (letfn [(get-seg [[x1 y1] [x2 y2]]
      (if (every? #(not (nil? %)) [x1 y1 x2 y2])
        (init-segment x1 y1 x2 y2)
        nil))]
    (fn [x y]
      (swap! points #(cons [x y] %))
      (let [[p1 p2 p3 p4] @points]
        (if (even? (count @points))
          [(get-seg p1 p2) (get-seg p3 p4)]
          [(get-seg p2 p3) nil]))))))

(defn setup []
  (background 0)
  (smooth)
  (stroke-weight 2)
  (set-state! 
    :line-color (color 10 200 100)
    :point-color (color 200 100 10)))

(defn mouse-pressed []
  (let [x (mouse-x) y (mouse-y)
        [seg1 seg2] (click x y)]
      (background 0)
      (stroke (state :line-color))
      (line-segment seg1)
      (line-segment seg2)
      (when-let [[x y] (get-intersection-point seg1 seg2)]
        (stroke (state :point-color))
        (fill (state :point-color))
        (ellipse x y 8 8))))

(defsketch geometry-lines
  :title "geometry lines - please click"
  :size [500 500]
  :setup setup
  :mouse-pressed mouse-pressed)

(defn -main [] ())
