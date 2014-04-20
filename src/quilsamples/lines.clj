(ns quilsample.lines
  (:use quil.core))

(defn setup []
  (smooth)
  (background 0)
  (stroke-weight 2)

  (doseq [x (range 0 500 10)]
    (stroke (random 255) (random 255) (random 255))
    (line x 0 x 300))
  
  (loop [z 0 y 300]
    (if(= z 600)
      nil
    (let [nx (+ z 10) ny (+ y (- (random 100) 50))]
      (do 
          (stroke  (random 255) (random 255) (random 255))
          (line z y nx ny)
          (recur nx ny)))))

  (loop [z 0 y 300]
    (if(= z 600)
      nil
    (let [nx (+ z 10) ny (+ y (- (* (noise (* z 0.01)) 25) 12))]
      (do (line z y nx ny)
          (recur nx ny))))))

(defsketch example
  :title "oh so many qrey circles"
  :setup setup
  :size [500 500])

(defn -main [] ())
