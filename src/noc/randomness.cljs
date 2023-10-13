(ns noc.randomness
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.math :as m]))

(defn- traditional-step-4-choices
  [[x y]]
  (case (rand-int 4)                                        ; idiom
    0 [(inc x) y]
    1 [(dec x) y]
    2 [x (inc y)]
    3 [x (dec y)]))

(defn- traditional-step-9-choices
  [xy]
  (m/v+ xy #(m/rand-int-off -1 2)))

(defn- traditional-step-infinite-choices
  [xy]
  (m/v+ xy #(m/rand-off -1 1)))

(defn traditional-random-walk [& {:keys [size fps]
                                  :or   {size [640 240]
                                         fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::traditional-random-walk size)]
    (-> ctx
        (c/set-fill-style :white)
        (c/fill-rect size)
        (c/set-fill-style :black))
    (go (loop [xy (m/v* size 0.5)]
          (>! in xy)
          (recur (traditional-step-4-choices xy))))
    (go (while true
          (c/fill-rect ctx (<! in) [1 1])
          (<! play)))
    ctrl))

(defn- make-bars [[w h] n counts]
  (let [bar-w (/ w n)]
    (map (fn [i c] [[(* i bar-w) h] [(dec bar-w) (* -1 c)]]) ; idiom
         (range)
         counts)))

(defn- distribution-bars [nm f & {:keys [size fps n]
                                  :or   {size [640 240]
                                         fps  30
                                         n    20}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append nm size)]
    (go (loop [counts (vec (repeat n 0))]
          (>! in counts)
          (recur (update counts (f n) inc))))
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

(defn random-distribution
  ([] (random-distribution {}))
  ([opts] (distribution-bars ::random-distribution rand-int opts)))

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
        ctx (c/append ::gaussian-distribution size)]
    (-> ctx
        (c/set-fill-style :white)
        (c/fill-rect size))
    (go (while true
          (-> ctx
              (c/set-fill-style "rgba(0,0,0,0.005)")
              (c/arc (update (m/v* size 0.5) 0 + (* (m/rand-std-norm) 60)) ; idiom
                     8
                     0
                     (m/pi 2))
              (c/fill))
          (<! play)))
    ctrl))

(defn- accept-reject []
  (let [r1 (rand) r2 (rand)]
    (if (< r2 r1) r1 (recur))))

(defn accept-reject-distribution
  ([] (accept-reject-distribution {}))
  ([opts] (distribution-bars ::accept-reject-distribution
                             #(js/Math.floor (* % (accept-reject)))
                             opts)))

(defn perlin-noise-walk [& {:keys [size fps]
                            :or   {size [640 240]
                                   fps  30}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::perlin-noise-walk size)]
    (-> ctx
        (c/set-fill-style :white)
        (c/fill-rect size)
        (c/set-fill-style "rgb(127,127,127)")
        (c/set-line-width 2))
    (go (loop [t [0 10000]]
          (>! in (m/v (* size 0.5 (+ (map m/noise-1d t) 1))))
          (recur (m/v+ 0.006 t))))
    (go (while true
          (let [xy (<! in)]
            (-> ctx
                (c/begin-path)
                (c/arc xy 24 0 (m/pi 2))
                (c/fill)
                (c/stroke))
            (<! play))))
    ctrl))
