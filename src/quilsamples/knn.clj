(ns quilsample.knn
  (:import [processing.core PVector])
  (:use quil.core))

(def group-num 10)
(def point-num 300)
(def max-x 700)
(def max-y 700)

(def points (take point-num (repeatedly #(new PVector (rand-int max-x) (rand-int max-y)))))
(def groups (atom (partition (/ point-num group-num) points)))
(def colors (for [x (range group-num)] (color (* x (/ 255 group-num)) 200 200)))

(defn draw-vector [v] (ellipse (.x v) (.y v) 7 7))
(defn draw-line [v1 v2] (line (.x v1) (.y v1) (.x v2) (.y v2)))
(defn get-center [vs] 
  (let [x (/ (reduce #(+ (.x %2) %1) 0 vs) (count vs))
        y (/ (reduce #(+ (.y %2) %1) 0 vs) (count vs))]
    (new PVector x y)))

(defn min-dist [cs p]
  (first (reduce (fn [[min-c min-d] c] 
            (let [d (.dist p c)]
              (if (<= d min-d) [c d] [min-c min-d])))
          [(new PVector 0 0) max-x] cs)))

(defn get-groups [cs ps] 
  (let [new-groups (group-by (partial min-dist cs) ps)]
      (for [[center group] new-groups]
        group)))

(def centers (atom (map get-center @groups)))

(defn setup []
  (frame-rate 1)
  (color-mode :hsb 255 255 255)
  (smooth)
  (stroke 250)
  (stroke-weight 1)
  (background 0))

(defn draw [] 
  (background 0)
  (dorun (for [[clr c ps] (map list colors @centers @groups)]
           (dorun (for [p ps]
             (do 
               (stroke clr)
               (fill clr)
               (draw-line c p)
               (draw-vector p)
               (draw-vector c))))))
  (reset! groups (get-groups @centers points))
  (reset! centers (map get-center @groups)))

(defsketch knn
  :title "knn"
  :setup setup
  :draw draw
  :size [max-x max-y])

(defn -main [] ())
