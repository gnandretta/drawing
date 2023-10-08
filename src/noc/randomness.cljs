(ns noc.randomness
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.math :as m]))

(defn- traditional-step-4-choices
  [[x y]]
  (case (rand-int 4)
    0 [(inc x) y]
    1 [(dec x) y]
    2 [x (inc y)]
    3 [x (dec y)]))

(defn- traditional-step-9-choices
  [xy]
  (mapv + xy (repeatedly #(m/rand-int-off -1 2))))

(defn- traditional-step-infinite-choices
  [xy]
  (mapv + xy (repeatedly #(m/rand-off -1 1))))

(defn traditional-random-walk [& {:keys [size fps]
                                  :or   {size [640 240]
                                         fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::traditional-random-walk size)]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect size)
        (c/set-fill-style "#000"))
    (go (loop [xy (m/v* size 0.5)]
          (>! in xy)
          (recur (traditional-step-4-choices xy))))
    (go (while true
          (c/fill-rect ctx (<! in) [1 1])
          (<! play)))
    ctrl))

(defn- make-bars [[w h] n counts]
  (let [bar-w (/ w n)]
    (map (fn [i c] [[(* i bar-w) h] [(dec bar-w) (* -1 c)]])
         (range)
         counts)))

(defn random-distribution [& {:keys [size fps n]
                              :or   {size [640 240]
                                     fps  30
                                     n    20}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::random-distribution size)]
    (go (loop [counts (vec (repeat n 0))]
          (>! in counts)
          (recur (update counts (rand-int n) inc))))
    (go (while true
          (-> ctx
              (c/set-fill-style :white)
              (c/fill-rect size)
              (c/set-fill-style "#7f7f7f")
              (c/set-line-width 2))
          (doseq [bar (make-bars size n (<! in))]
            (-> ctx
                (c/begin-path)
                (c/apply c/rect bar)
                (c/fill)
                (c/stroke)))
          (<! play)))
    ctrl))

(defn- tends-to-right-step
  [[x y]]
  (condp >= (rand)                                          ; idiom
    0.4 [(inc x) y]
    0.6 [(dec x) y]
    0.8 [x (inc y)]
    1 [x (dec y)]))

(defn random-walk-tends-to-right [& {:keys [size fps]
                                     :or   {size [640 240]
                                            fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::random-walk-tends-to-right size)]
    (go (loop [xy (m/v* size 0.5)]
          (>! in xy)
          (recur (tends-to-right-step xy))))
    (-> ctx
        (c/set-fill-style :white)
        (c/fill-rect size))
    (go (while true
          (-> ctx
              (c/set-fill-style :black)
              (c/fill-rect (<! in) [1 1]))
          (<! play)))
    ctrl))

(defn gaussian-distribution [& {:keys [size fps]
                                :or   {size [640 240]
                                       fps  30}}]
  (let [[play ctrl] (a/play fps)
        ctx (c/append ::gaussian-distribution size)
        {:keys [d]} (c/layout :size size)]
    (-> ctx
        (c/set-fill-style :white)
        (c/fill-rect size))
    (go (while true
          (-> ctx
              (c/set-fill-style "rgba(0,0,0,0.005)")
              (c/arc (update (d 0.5) 0 + (* (m/rand-std-norm) 60)) ; idiom
                     8
                     0
                     (m/pi 2))
              (c/fill))
          (<! play)))
    ctrl))

(defn accept-reject-distribution [& {:keys [size fps n]
                                     :or   {size [640 240]
                                            fps  30
                                            n    20}}]
  (let [accept-reject (fn [] (let [r1 (rand) r2 (rand)]
                               (if (< r2 r1) r1 (recur))))
        in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::accept-reject-distribution size)]
    (go (loop [counts (vec (repeat n 0))]
          (>! in counts)
          (recur (update counts (js/Math.floor (* n (accept-reject))) inc))))
    (go (while true
          (-> ctx
              (c/set-fill-style :white)
              (c/fill-rect size)
              (c/set-fill-style "#7f7f7f")
              (c/set-stroke-style :black)
              (c/set-line-width 2))
          (doseq [bar (make-bars size n (<! in))]
            (-> ctx
                (c/begin-path)
                (c/apply c/rect bar)
                (c/fill)
                (c/stroke)))
          (<! play)))
    ctrl))

(defn perlin-noise-walk [& {:keys [size fps]
                            :or   {size [640 240]
                                   fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        [w h] size
        ctx (c/append ::perlin-noise-walk size)]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect size))
    (go (loop [tx 0 ty 10000]
          (>! in [(* w (+ 1 (m/noise-1d tx)) 0.5)
                  (* h (+ 1 (m/noise-1d ty)) 0.5)])
          (recur (+ tx 0.006) (+ ty 0.006))))
    (go (while true
          (let [p (<! in)]
            (-> ctx
                (c/set-fill-style "rgb(127,127,127)")
                (c/set-line-width 2)
                (c/begin-path)
                (c/arc p 24 0 (m/pi 2))
                (c/fill)
                (c/stroke))
            (<! play))))
    ctrl))
