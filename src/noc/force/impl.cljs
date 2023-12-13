(ns noc.force.impl
  (:require [drawing.canvas :as c]
            [drawing.math :as m]))

(defn- make [& {:as attrs}]
  (let [mass (get attrs :mass 1)]
    (merge {:xy   [0 0]
            :r    (* 8 mass)
            :mass mass
            :a    [0 0]
            :v    [0 0]}
           attrs)))

(defn make-mover [& {:as attrs}]
  (make attrs))

(defn draw-mover [ctx m]
  (let [line-width 2]
    (-> ctx
        (c/save)
        (c/set-fill-style "rgba(127,127,127,0.5)")
        (c/set-stroke-style :black)
        (c/set-line-width line-width)
        (c/begin-path)
        (c/circle (:xy m) (- (:r m) line-width))
        (c/fill)
        (c/stroke)
        (c/restore))))

(defn bounce                                                ; TODO how to model this using a force?
  ([m d] (bounce m d 1))
  ([{:keys [xy v] :as m} [w h] v-ratio]                     ; ratio of conserved energy/speed
   (let [[vx vy] v
         [x y] xy
         [x vx] (cond (> x w) [w (* -1 v-ratio vx)]
                      (< x 0) [0 (* -1 v-ratio vx)]
                      :else [x vx])
         [y vy] (if (> y h) [h (* -1 v-ratio vy)] [y vy])]
     (merge m {:xy [x y] :v [vx vy]}))))

(defn move
  [{:keys [v xy mass] :as m} forces]
  (let [a (apply m/add (map #(m/div % mass) forces))
        v (m/add v a)
        xy (m/add xy v)]
    (merge m {:xy xy :v v :a a})))

(defn make-attractor
  [xy]
  (make :xy xy :mass 20))

(defn draw-attractor
  [ctx m]
  (-> ctx
      (c/save)
      (c/set-fill-style "rgba(125,125,125,0.78)")
      (c/set-stroke-style :black)
      (c/set-line-width 4)
      (c/begin-path)
      (c/circle (:xy m) (:mass m))                          ; TODO adjust numbers
      (c/fill)
      (c/stroke)
      (c/restore)))

(defn get-attraction                                       ; force experienced by b due to a's attraction
  [a b]
  (let [dist (m/sub (:xy a) (:xy b))
        mag-dist (-> (m/mag dist) (max 5) (min 25))]
    (m/mul (m/normalize dist) (/ (* (:mass a) (:mass b)) (* mag-dist mag-dist)))))

(defn make-body
  [attrs]
  (make (merge {:mass 8} attrs)))

(defn draw-body [ctx m]
  (-> ctx
      (c/save)
      (c/set-fill-style "rgba(127,127,127,0.5)")
      (c/set-stroke-style :black)
      (c/set-line-width 2)
      (c/begin-path)
      (c/circle (:xy m) (:r m))
      (c/fill)
      (c/stroke)
      (c/restore)))
