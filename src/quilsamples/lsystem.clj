(ns quilsamples.lsystem
  (:use quil.core))

(def turtles (atom []))

(defrecord Turtle [x y angle pen])

(defn init-turtle [x y]
  (->Turtle x y (radians 0) true))

(defn push-turtle! [turtle]
  (swap! turtles #(conj % turtle))
  turtle)

(defn pop-turtle! []
  (let [turtle (peek @turtles)]
    (reset! turtles (pop @turtles))
    turtle))
        
(def get-next-color! 
  (let [hue (atom 0)
        next-hue (fn [h] (if (<= 100.0 h) 0.0 (+ h 0.1)))]
    (fn [] (color (swap! hue next-hue) 80 80))))

(defn get-new-coordinates [x y angle dist]
  (let [new-x (+ (* (cos angle) dist) x)
        new-y (+ (* (sin angle) dist) y)]
    [new-x new-y]))

(defn penup [t]
  (assoc t :pen false))
(defn pendown [t] 
  (assoc t :pen true))

(defn foward [dist t] 
  (let [x (:x t)
        y (:y t)
        angle (+ (:angle t) PI)
        [new-x new-y] (get-new-coordinates x y angle dist)]
    (when (true? (:pen t))
      (stroke (get-next-color!))
      (line x y new-x new-y))
    (assoc t :x new-x :y new-y)))

(defn backward [dist t] (foward (- dist) t))

(defn right [angle t] 
  (assoc t :angle (+ (:angle t) (radians angle))))

(defn left [angle t] (right (- angle) t))

(def translation-table
  {:f (partial #(->> % (pendown) (foward 2)))
   :r (partial right 90)
   :l (partial left 90)})

(def rewrite-rule 
  {:f [:f :r :f :l :f :l :f :r :f]})

(def tree-translation-table
  {:f (partial #(->> % (pendown) (foward 8)))
   :r (partial right 20)
   :l (partial left 20)
   :i push-turtle!
   :o (fn [t] (pop-turtle!))})

(def rewrite-tree-rule
  {:f [:f :f :l :i :l :f :f :f :o :r :i :r :f :l :f :l :f :o]})

(defn rewrite [cells rule]
    (loop [source cells dest []]
      (if (empty? source)
        (flatten dest)
        (let [[fst & rst] source
              cell (if-let [c (fst rule)] c fst)]
          (recur rst (conj dest cell))))))

(defn rewrite-n [cells rule n]
  (if (= n 0)
    (rewrite cells rule)
    (recur (rewrite cells rule) rule (dec n))))

(defn draw-cells [cells turtle table]
  (loop [source cells trtl turtle]
    (if (empty? source)
      trtl
      (let [[fst & rst] source
            action (fst table)]
        (recur rst (action trtl))))))

(defn split-until [w coll]
  (let [i (.indexOf coll w)]
    (if (= i -1)
      [coll []]
      [(take (inc i) coll) (drop (inc i) coll)])))

(defn split-move-n [actions n]
  (loop [moves [] rest-actions actions times n]
    (if (= times 0)
      [moves rest-actions]
      (let [[move nexts] (split-until :f rest-actions)]
        (recur (concat moves move) nexts (dec times))))))

(defn setup [] 
  (smooth)
  (background 0)
  (stroke-weight 1)
  (color-mode :hsb 100 100 100)
  (set-state! :order (atom (rewrite-n [:f] rewrite-rule 4))
              :turtle (atom (init-turtle 500 500))
              :tree-order (atom (rewrite-n [:f] rewrite-tree-rule 3))
              :tree-turtle (atom (right 90 (init-turtle 250 500)))))

(defn draw []
  (let [order (state :order)
        turtle (state :turtle)
        [current rest-cells] (split-move-n @order 10)]
    (swap! turtle #(draw-cells current % translation-table))
    (reset! order rest-cells))
  
  (let [order (state :tree-order)
        turtle (state :tree-turtle)
        [current rest-cells] (split-move-n @order 10)]
    (swap! turtle #(draw-cells current % tree-translation-table))
    (reset! order rest-cells)))

(defsketch lsystem 
  :title "LSystem"
  :size [500 500]
  :setup setup
  :draw draw)

(defn -main [] ())
