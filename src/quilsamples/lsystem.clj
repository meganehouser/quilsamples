(ns quilsample.lsystem
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
      (line x y new-x new-y))
    (assoc t :x new-x :y new-y)))

(defn backward [dist t] (foward (- dist) t))

(defn right [angle t] 
  (assoc t :angle (+ (:angle t) (radians angle))))

(defn left [angle t] (right (- angle) t))

(def translation-table
  {:f (partial #(->> % (pendown) (foward 2)))
   :b (partial backward 5)
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
        

(defn setup [] 
  (smooth)
  (stroke-weight 1)
  (stroke (color 200 100 0))
  (-> [:f]
      (rewrite-n 4)
      (draw-cells (init-turtle 500 500))))

(defsketch recurtree
  :title "tree"
  :size [500 500]
  :setup setup)

(defn -main [] ())
