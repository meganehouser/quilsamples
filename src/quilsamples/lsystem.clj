(ns quilsamples.lsystem
  (:use quil.core))

(defrecord Turtle [x y angle pen])

(defn init-turtle [x y]
  (->Turtle x y (radians 0) true))

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
   :b (partial #(->> % (penup) (backward 5)))
   :r (partial right 90)
   :l (partial left 90)})

(def rewrite-rule 
  {:f [:f :r :f :l :f :l :f :r :f]})

(defn rewrite [cells]
    (loop [source cells dest []]
      (if (empty? source)
        (reverse (flatten dest))
        (let [[fst & rst] source
              cell (if-let [c (fst rewrite-rule)] c fst)]
          (recur rst (cons cell dest))))))

(defn rewrite-n [cells n]
  (if (= n 0)
    (rewrite cells)
    (recur (rewrite cells) (dec n))))

(defn draw-cells [cells turtle]
  (loop [source cells trtl turtle]
    (if (empty? source)
      trtl
      (let [[fst & rst] source
            action (fst translation-table)]
        (recur rst (action trtl))))))
        
(def get-next-color! 
  (let [hue (atom 0)
        next-hue (fn [h] (if (<= 100.0 h) 0.0 (+ h 0.1)))]
    (fn [] (color (swap! hue next-hue) 80 80))))

(defn split-move [actions]
  (let [[current rest-actions] (split-with #(not (= :f %)) actions)]
    (condp = (first rest-actions)
      :f [(concat current [:f]) (rest rest-actions)]
      nil [current ()]
      [current rest-actions])))

(defn split-move-n [actions n]
  (loop [moves [] rest-actions actions times n]
    (if (= times 0)
      [moves rest-actions]
      (let [[move nexts] (split-move rest-actions)]
        (recur (concat moves move) nexts (dec times))))))

(defn setup [] 
  (smooth)
  (background 0)
  (stroke-weight 1)
  (color-mode :hsb 100 100 100)
  (set-state! :order (atom (rewrite-n [:f] 4))
              :turtle (atom (init-turtle 500 500))))

(defn draw []
  (let [order (state :order)
        turtle (state :turtle)
        [current rest-cells] (split-move-n @order 10)]
    (swap! turtle #(draw-cells current %))
    (reset! order rest-cells)))

(defsketch lsystem 
  :title "LSystem"
  :size [500 500]
  :setup setup
  :draw draw)

(defn -main [] ())
