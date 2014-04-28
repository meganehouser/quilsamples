(ns quilsamples.geometry.geometry
  (:use quil.core))

; from http://gihyo.jp/dev/serial/01/geometry

(defrecord Geo-line [a b c])

(defn init-line [a b c] (->Geo-line a b c))

(defn from-points [x1 y1 x2 y2]
  "二点を通る直線"
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (->Geo-line dy (- dx) (- (* dx y1) (* dy x1)))))


(defrecord Geo-linesegment [x1 y1 x2 y2])

(defn init-segment [x1 y1 x2 y2] (->Geo-linesegment x1 y1 x2 y2))

(defn to-line [l-segment]
  "線分を直線に変換する"
  (from-points (:x1 l-segment) (:y1 l-segment) (:x2 l-segment) (:y2 l-segment)))

(defmulti intersects? #(map class (vec [%1 %2])))

(defmethod intersects? [Geo-linesegment Geo-line] [l-segment gline]
  "線分と直線の交差判定"
  (let [t1 (+ (* (:a gline) (:x1 l-segment)) (* (:b gline) (:y1 l-segment)) (:c gline))
        t2 (+ (* (:a gline) (:x2 l-segment)) (* (:b gline) (:y2 l-segment)) (:c gline))]
    (<= (* t1 t2) 0)))

(defmethod intersects? [Geo-linesegment Geo-linesegment] [lseg1 lseg2]
  "2線分の交差判定"
  (and (intersects? lseg1 (to-line lseg2)) (intersects? lseg2 (to-line lseg1))))

(defmethod intersects? :default [_ _] nil)

(defmulti get-intersection-point #(map class (vec [%1 %2])))

(defmethod get-intersection-point [Geo-line Geo-line] [l1 l2]
  "２直線の交点を求める"
  (let [d (- (* (:a l1) (:b l2)) (* (:a l2) (:b l1)))]
    (if (= d 0)
      nil
      (let [x (/ (- (* (:b l1) (:c l2)) (* (:b l2) (:c l1))) d)
            y (/ (- (* (:a l2) (:c l1)) (* (:a l1) (:c l2))) d)]
        [x y]))))

(defmethod get-intersection-point [Geo-linesegment Geo-line] [lseg gline]
  "線分と直線の交点を求める"
  (when (intersects? lseg gline)
    (get-intersection-point (to-line lseg) gline)))

(defmethod get-intersection-point [Geo-linesegment Geo-linesegment] [lseg1 lseg2]
  "２線分の交点を求める"
  (when (intersects? lseg1 lseg2)
    (get-intersection-point (to-line lseg1) (to-line lseg2))))

(defmethod get-intersection-point :default [_ _] nil)

(defn main [] ())
