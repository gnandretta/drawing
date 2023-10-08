(ns noc.randomness
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.math :as m]))

(defn- traditional-step-4-choices [[x y]]
  (let [choice (js/Math.floor (rand 4))]
    (case choice
      0 [(inc x) y]
      1 [(dec x) y]
      2 [x (inc y)]
      3 [x (dec y)])))

(defn- traditional-step-9-choices [[x y]]
  [(+ x (js/Math.floor (rand 3)) -1)
   (+ y (js/Math.floor (rand 3)) -1)])

(defn- traditional-step-infinite-choices [[x y]]
  [(+ x (rand 2) -1) (+ y (rand 2) -1)])

(defn traditional-random-walk [& {:keys [size fps]
                                  :or   {size [640 240]
                                         fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append "traditional-random-walk" size)]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go (loop [p (mapv (partial * 0.5) size)]
          (>! in p)
          (recur (traditional-step-4-choices p))))
    (go (while true
          (-> ctx
              (c/set-fill-style "red")
              (c/fill-rect (<! in) [1 1]))
          (<! play)))
    ctrl))

(defn random-distribution [& {:keys [size fps n]
                              :or   {size [640 240]
                                     fps  30
                                     n    20}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append "random-distribution" size)
        [w h] size
        w-bar (/ w n)]
    (go (loop [counts (vec (repeat n 0))]
          (>! in counts)
          (recur (update counts (rand-int n) inc))))
    (go (while true
          (-> ctx
              (c/set-fill-style "#fff")
              (c/fill-rect [0 0] size)
              (c/set-fill-style "#7f7f7f")
              (c/set-stroke-style "#000")
              (c/set-line-width 2))
          (let [counts (<! in)]
            (doseq [i (range 0 n)]
              (-> ctx
                  (c/begin-path)
                  (c/rect [(* i w-bar) h] [(dec w-bar) (* (counts i) -1)])
                  (c/fill)
                  (c/stroke))))
          (<! play)))
    ctrl))

(defn random-walk-tends-to-right [& {:keys [size fps]
                                     :or   {size [640 240]
                                            fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        step (fn [[x y]]
               (condp >= (rand)
                 0.4 [(inc x) y]
                 0.6 [(dec x) y]
                 0.8 [x (inc y)]
                 1 [x (dec y)]))
        ctx (c/append "random-walk-tends-to-right" size)]
    (go (loop [p (mapv (partial * 0.5) size)]
          (>! in p)
          (recur (step p))))
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go (while true
          (-> ctx
              (c/set-fill-style "#000")
              (c/fill-rect (<! in) [1 1]))
          (<! play)))
    ctrl))

(defn gaussian-distribution [& {:keys [size fps]
                                :or   {size [640 240]
                                       fps  30}}]
  (let [[play ctrl] (a/play fps)
        [w h] size
        ctx (c/append "gaussian-distribution" size)]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go (while true
          (-> ctx
              (c/set-fill-style "rgba(0,0,0,0.005)")
              (c/arc [(+ (* (m/rand-std-norm) 60) (* w 0.5)) (* h 0.5)]
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
        ctx (c/append "accept-reject-distribution" size)
        [w h] size
        w-bar (/ w n)]
    (go (loop [counts (vec (repeat n 0))]
          (>! in counts)
          (recur (update counts (js/Math.floor (* n (accept-reject))) inc))))
    (go (while true
          (-> ctx
              (c/set-fill-style "#fff")
              (c/fill-rect [0 0] size)
              (c/set-fill-style "#7f7f7f")
              (c/set-stroke-style "#000")
              (c/set-line-width 2))
          (let [counts (<! in)]
            (doseq [i (range 0 n)]
              (-> ctx
                  (c/begin-path)
                  (c/rect [(* i w-bar) h] [(dec w-bar) (* (counts i) -1)])
                  (c/fill)
                  (c/stroke))))
          (<! play)))
    ctrl))

(defn perlin-noise-walk [& {:keys [size fps]
                            :or   {size [640 240]
                                   fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        [w h] size
        ctx (c/append "perlin-noise-walk" size)]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
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
