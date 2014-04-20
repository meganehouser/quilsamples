(ns quilsample.monocro
  (:use quil.core))

(def img (atom nil))
(def dest (atom nil))
(def threshold 110.0)

(defn pred-color [b elem bc]
  (let [[low high new-color] elem]
    (if (< low b high)
      new-color
      nil)))

(defn get-color [c colors bc]
  (let [b (brightness c)]
    (if-let [new-color (some #(pred-color b % bc) colors)]
      new-color
      bc)))

(defn setup []
  (reset! img (load-image "resources/avatar.jpeg"))
  (reset! dest (create-image (. @img width) (. @img height) 1))
  (set-state! :base-color (color 245 242 241)
               :colors [[0 60 (color 124 189 161)]
                        [60 140 (color 218 128 125)]
                        [200 500 (color 243 200 180)]]))


(defn draw []
  (. @img loadPixels)
  (. @dest loadPixels)
  (let [colors (state :colors)
        base-color (state :base-color)]
    (dorun 
      (for [index (range 193599)]
        (let [pix  (aget (. @img pixels) index)
              new-color (get-color pix colors base-color)]
            (aset (. @dest pixels) index new-color)))))
  (. @dest updatePixels)
  (image @dest 0 0)
  (no-loop))


(defsketch monocro
  :title "monocro"
  :setup setup
  :draw draw
  :renderer :p3d
  :size [500 500])

(defn -main [] ())
