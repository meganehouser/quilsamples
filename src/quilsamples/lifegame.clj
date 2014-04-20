(ns quilsamples.lifegame
  (:use quil.core
        quilsamples.lifegame.conway))


(defn create-randworld [w h rnd]
  (for [x (range w) y (range h)
    :when (> 1 (rnd 0 4))]
    [x y]))

(defn setup []
  (background 0)
  (set-state! :world (atom (set (create-randworld 270 180 random)))
              :cnt (atom 0)))

(defn draw []
  (background 10)
  (smooth)
  (no-stroke)
  (fill (color 170 80))
  (let [cnt (swap! (state :cnt) inc)
        pattern (state :world)
        cells (vec (conway2 [100 100] @pattern ))]
    (dorun
      (for [[x y] cells]
       (ellipse (* x 5) (* y 5) 25 25)))
    (reset! (state :world) (set cells))))

(defsketch lefegame 
  :title "life game"
  :size [500 500]
  :setup setup
  :draw draw)
