(ns quilsamples.geometry
  (:use quil.core))

; from http://gihyo.jp/dev/serial/01/geometry

(defrecord Geoline [a b c])

(defn from-points [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (->Geoline dy (- dx) (- (* dx y1) (* dy x1)))))

(defn get-intersection-point [l1 l2]
  (let [d (- (* (:a l1) (:b l2)) (* (:a l2) (:b l1)))]
    (if (= d 0)
      nil
      (let [x (/ (- (* (:b l1)  (:c l2)) (* (:b l2) (:c l1))) d)
            y (/ (- (* (:a l2) (:c l1)) (* (:a l1) (:c l2))) d)]
        [x y]))))


(defn main [] ())
