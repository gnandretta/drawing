(ns noc.randomness
  (:require [cljs.core.async :as a :refer [<!] :refer-macros [go go-loop]]
            [drawing.canvas :as c]
            [drawing.math :as m]))

(defn traditional-random-walk [& {:keys [size fps]
                                  :or   {size [640 240]
                                         fps  30}}]
  (let [step (fn [[x y]]                                    ; 4 choices
               (let [choice (js/Math.floor (rand 4))]
                 (case choice
                   0 [(inc x) y]
                   1 [(dec x) y]
                   2 [x (inc y)]
                   3 [x (dec y)])))
        #_#_step (fn [[x y]]                                ; 9 choices
                   [(+ x (js/Math.floor (rand 3)) -1)
                    (+ y (js/Math.floor (rand 3)) -1)])
        #_#_step (fn [[x y]]                                ; infinite choices
                   [(+ x (rand 2) -1) (+ y (rand 2) -1)])
        ctx (c/ctx (c/create "traditional-random-walk" size))]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go-loop [p (mapv (partial * 0.5) size)]
      (-> ctx
          (c/set-fill-style "#000")
          (c/fill-rect p [1 1]))
      (<! (a/timeout (/ 1000 fps)))
      (recur (step p)))))

(defn random-distribution [& {:keys [size fps n]
                              :or   {size [640 240]
                                     fps  30
                                     n    20}}]
  (let [ctx (c/ctx (c/create "random-distribution" size))
        [w h] size
        w-bar (/ w n)]
    (go-loop [counts (vec (repeat n 0))]
      (-> ctx
          (c/set-fill-style "#fff")
          (c/fill-rect [0 0] size)
          (c/set-fill-style "#7f7f7f")
          (c/set-stroke-style "#000")
          (c/set-line-width 2))
      (doseq [i (range 0 n)]
        (-> ctx
            (c/begin-path)
            (c/rect [(* i w-bar) h] [(dec w-bar) (* (counts i) -1)])
            (c/fill)
            (c/stroke)))
      (<! (a/timeout (/ 1000 fps)))
      (recur (update counts (rand-int n) inc)))))

(defn random-walk-tends-to-right [& {:keys [size fps]
                                     :or   {size [640 240]
                                            fps  30}}]
  (let [step (fn [[x y]]
               (condp >= (rand)
                 0.4 [(inc x) y]
                 0.6 [(dec x) y]
                 0.8 [x (inc y)]
                 1 [x (dec y)]))
        ctx (c/ctx (c/create "random-walk-tends-to-right" size))]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go-loop [p (mapv (partial * 0.5) size)]
      (-> ctx
          (c/set-fill-style "#000")
          (c/fill-rect p [1 1]))
      (<! (a/timeout (/ 1000 fps)))
      (recur (step p)))))

(defn gaussian-distribution [& {:keys [size fps]
                                :or   {size [640 240]
                                       fps  30}}]
  (let [[w h] size
        ctx (c/ctx (c/create "gaussian-distribution" size))]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go (while true
          (-> ctx
              (c/set-fill-style "rgba(0,0,0,0.005)")
              (c/arc (+ (* (m/rand-std-norm) 60) (* w 0.5))
                     (* h 0.5)
                     8
                     0
                     (* js/Math.PI 2))
              (c/fill))
          (<! (a/timeout (/ 1000 fps)))))))

(defn accept-reject-distribution [& {:keys [size fps n]
                                     :or   {size [640 240]
                                            fps  30
                                            n    20}}]
  (let [accept-reject (fn [] (loop []
                               (let [r1 (rand)
                                     r2 (rand)]
                                 (if (< r2 r1)
                                   r1
                                   (recur)))))
        ctx (c/ctx (c/create "accept-reject-distribution" size))
        [w h] size
        w-bar (/ w n)]
    (go-loop [counts (vec (repeat n 0))]
      (-> ctx
          (c/set-fill-style "#fff")
          (c/fill-rect [0 0] size)
          (c/set-fill-style "#7f7f7f")
          (c/set-stroke-style "#000")
          (c/set-line-width 2))
      (doseq [i (range 0 n)]
        (-> ctx
            (c/begin-path)
            (c/rect [(* i w-bar) h] [(dec w-bar) (* (counts i) -1)])
            (c/fill)
            (c/stroke)))
      (<! (a/timeout (/ 1000 fps)))
      (recur (update counts (js/Math.floor (* n (accept-reject))) inc)))))

(defn perlin-noise-walk [& {:keys [size fps]
                            :or   {size [640 240]
                                   fps  30}}]
  (let [[w h] size
        ctx (c/ctx (c/create "perlin-noise-walk" size))]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go-loop [tx 0 ty 10000]
      (-> ctx
          (c/set-fill-style "rgb(127,127,127)")
          (c/set-line-width 2)
          (c/begin-path)
          (c/arc (* w (+ 1 (m/noise tx)) 0.5)
                 (* h (+ 1 (m/noise ty)) 0.5)
                 24
                 0
                 (* 2 js/Math.PI))
          (c/fill)
          (c/stroke))
      (<! (a/timeout (/ 1000 fps)))
      (recur (+ tx 0.01) (+ ty 0.01)))))
