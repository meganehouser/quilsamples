(ns quilsample.anime2
  (:use quil.core))

(def seed (atom 0.0))

(defn draw-skrew []
  (dorun 
    (for [i (range (/ 360 2))]
      (let [x (+ (* (sin i) (* 2 i)))
            y (+ (* (cos i) (* 2 i)))]
        (rect x y 10 10)))))

(defn setup [] 
  (frame-rate 10)
  (smooth)
  (no-stroke)
  (fill 90))

(defn draw [] ()
  (background 255)
  (translate (/ (width) 2) (/ (height) 2))
  (rotate (swap! seed + 0.5))
  (fill (color 100 50 10))
  (dorun
    (for [i (range (/ 360 2))]
      (let [x 0
            y (* i 2)]
        (rotate 0.5)
        (rect x y 10 10)))))

(defsketch anime2
  :title "whirlpool anime"
  :setup setup
  :draw draw
  :size [500 500])

(defn -main [] ())
