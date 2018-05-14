(ns ninety-nine-clojure.visual
  (:require [quil.core :as q]
            [ninety-nine-clojure.bintrees :as b]))



(defn draw-node [{:keys [v x y]} s]
  (q/fill 255 255 255)
  (q/ellipse x y  s s)
  (q/fill 0 0 0)
  (q/stroke 0 0 0)
  (q/text-size (/ s 1.5))
  (q/text (str v) (- x (/ s 4)) (+ y (/ s 4) )  ))


(defn point-on-circle [x1 y1 r ang]
  (let [x (* r (q/cos ang))
        y (* r (q/sin ang))]
    [(+ x1 x) (+ y1 y)]))

(defn draw-edge [{x1 :x y1 :y} [{x2 :x y2 :y} _ _] size d]
  (when (and x2 y2)
    (let [ang (if (= :r d) 45 90)
          [x y] (point-on-circle x1 y1 (/ size 2) ang)]
      (q/line x y x2 y2)
      (q/ellipse x2 y2 5 5))))

(defn draw-tree 
  [size [n l r]]
  (when n
    (draw-node n size)
    (when l
      (draw-edge n l size :l)
      (draw-tree size l))
    (when r
      (draw-edge n r size :r)
      (draw-tree size r))))

(defn scale-tree [t size]
  (b/depth-first-tree-edit (b/tree-zip t)
                           (fn [[v & children :as node] state]
                             (if node
                               [(apply vector (-> v
                                                  (update :x * size)
                                                  (update :y * size)) children) state]
                               [node state]))))


(defn draw-grid [size]
  (q/with-fill [236]
    (q/with-stroke [236]
      (q/text-size (* 0.5 size))
      (let [xs (range size (q/width) size)
            ys (range size (q/height) size)]
        (doseq [x xs]
          (q/text-align :center :baseline)
          (q/text (str (/ x size)) x (/ size 2))
          (q/line x (* size 0.75) x (q/height)))
        (doseq [y ys]
          (q/text-align :center :center)
          (q/text (str (/ y size)) (/ size 3) y)
          (q/line (* size 0.75) y (q/width) y))))
    
    (q/text-align :left :baseline)))


(defn draw []
  (let [;_     (q/exit)
        size 30
        tree (b/layout3 '[n [k [c [a nil nil] [e [d nil nil] [g nil nil]]] [m nil nil]] [u [p nil [q nil nil]] nil]])
        ;; tree (b/layout3 '[a
        ;;              [b
        ;;               [c  nil [d nil nil]]
        ;;               [e nil [f nil nil]]]
        ;;              [g, [h nil nil] nil]])
        ]
    (q/background 255)
    (draw-grid size)
    (draw-tree size (scale-tree tree size) )
    ;; (q/with-translation  [(/ (q/width) 2) (/ (q/height) 2)]
    ;;         )
    ))

(q/defsketch tree
  :features [:no-bind-output]
  :size [700 400]
  :draw draw )


