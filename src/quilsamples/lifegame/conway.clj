(ns lifegame.conway)


(defn create-world [w h & living-cells]
  (vec (for [y (range w) x (range h)
                :when (contains? (first living-cells) [x y])]
         [x y])))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn stepper [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n)) ]
           loc))))

(def conway-stepper (stepper neighbours #{3} #{2 3}))
(def mconway-stepper (memoize conway-stepper))

(defn conway2 [[w h] pattern]
  (->> (conway-stepper pattern)
       (create-world w h)))

(defn conway [[w h] pattern iterations]
  (->> (iterate mconway-stepper pattern)
       (drop iterations)
       first
       (create-world w h)))
