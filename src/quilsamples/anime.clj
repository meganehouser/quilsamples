(ns quilsample.anime
  (:use quil.core))

(def size-height 300)
(def size-width 300)
(def xstart (atom 0))
(def ystart (atom 0))

(defrecord Element [noise pos])
(defn elm-list [noise-start max-size]
  (map #(hash-map :noise (+ noise-start (* % 0.01)) :pos %) (range 0 max-size 5)))

(defn draw-point [x y noiseFactor]
  (push-matrix)
  (translate x y)
  (rotate (* noiseFactor (radians 360)))
  (stroke 0 150)
  (line 0 0 20 0)
  (pop-matrix))

(defn draw-point2 [x y noiseFactor]
  (push-matrix)
  (translate x y)
  (rotate (* noiseFactor (radians 540)))
  (let [edge-size (* noiseFactor 35)
        grey (+ 150 (* noiseFactor 120))
        alpha (+ 150 (* noiseFactor 120))]
    (no-stroke)
    (fill grey alpha)
    (ellipse 0 0 edge-size (/ edge-size 2.0))
  (pop-matrix)))

(defn draw []
  (smooth)
  (background 255)
  (swap! xstart #(+ 0.01 %))
  (swap! ystart #(+ 0.01 %)) 
    (dorun 
      (for [x (elm-list @xstart size-width) y (elm-list @ystart size-height)]
        (draw-point2 (:pos x) (:pos y) (noise (:noise x) (:noise y))))))


(defn setup []
  (reset! xstart (random 10))
  (reset! ystart (random 10))
  )

(defsketch lines
  :title "perlin noise cloud anime"
  :setup setup
  :draw draw
  :size [size-width size-height])

(defn -main [] ())
