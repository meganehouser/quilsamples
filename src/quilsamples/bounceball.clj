(ns quilsample.bounceball
  (:use quil.core))

(def get-next-color! 
  (let [hue (atom 0)
        next-hue (fn [h] (if (<= 100.0 h) 0.0 (+ h 0.3)))]
    (fn [] (color (swap! hue next-hue) 80 100))))

(def get-next-y!
  (let [angle (atom 0)
        step (/ TWO-PI 100.0)
        next-angle (fn [a] (if (= a TWO-PI) 0.0 (+ a step)))]
    (fn [] (* (sin (swap! angle next-angle)) 200))))

(def get-next-radius!
  (let [radius (atom 0)
        step (/ TWO-PI 100.0)
        next-radius (fn [a] (if (= a TWO-PI) 0.0 (+ a step)))]
    (fn [] (+ (abs (* (sin (swap! radius next-radius)) 50)) 50))))

(def get-next-rotate!
  (let [r (atom 0)
        step (/ TWO-PI 600.0)
        next-rotate (fn [x] (if (= x TWO-PI) 0.0 (+ x step)))]
    (fn [] (swap! r next-rotate))))

(defn set-translate []
  (translate (/ (width) 2) (/ (height) 2)))

(defn setup []
  (frame-rate 25)
  (background 0)
  (color-mode :hsb 100 100 100)
  (smooth)
  (no-stroke))

(defn draw [] 
  (set-translate)
  (rotate (get-next-rotate!))
  (fill (get-next-color!))
  (background 0)
  (let [r (get-next-radius!)]
  (ellipse 0 (get-next-y!) r r)))

(defsketch bounce-ball
  :title "はねるボール"
  :size [500 500]
  :setup setup
  :draw draw)

(defn -main [] ())
