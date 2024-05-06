(ns noc.randomness
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.math :as m :include-macros true]))

(defn traditional-random-walk [& {:keys [d] :or {d [640 240]}}] ; example 0.1
  (let [ctx (c/append ::traditional-random-walk d)]
    (go (loop [[x y :as xy] (m/mul 0.5 d)]
          (c/fill-rect ctx xy [1 1])
          (<! (timeout 16))
          (recur (case (rand-int 4)                         ; idiom, also try (m/add xy #(m/rand-int-off -1 2)) and (m/add xy #(m/rand-off -1 1))
                   0 [(inc x) y]
                   1 [(dec x) y]
                   2 [x (inc y)]
                   3 [x (dec y)]))))))

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

(defn random-number-distribution [& {:keys [d] :or {d [640 240]}}] ; example 0.2
  (let [[w h] d
        ctx (c/append ::random-distribution d)
        n 20
        rects (let [w' (/ w n)] (vec (for [i (range n)] [[(* i w') h] [w' h]])))]
    (go (loop [rects rects]
          (doseq [[xy [w* h*]] rects]
            (-> ctx
                (c/set-fill-style "#7F7F7F")
                (c/set-line-width 2)
                (c/begin-path)
                (c/rect xy [(dec w*) h*])
                (c/fill)
                (c/stroke)))
          (<! (timeout 16))
          (recur (update-in rects [(rand-int n) 0 1] dec))))))

(defn random-walk-tends-to-right [& {:keys [d] :or {d [640 240]}}] ; example 0.3
  (let [ctx (c/append ::random-walk-tends-to-right d)]
    (go (loop [[x y :as xy] (m/mul 0.5 d)]
          (c/fill-rect ctx xy [1 1])
          (<! (timeout 16))
          (recur (condp > (rand)                            ; idiom
                   0.4 [(inc x) y]
                   0.6 [(dec x) y]
                   0.8 [x (inc y)]
                   1 [x (dec y)]))))))

(defn gaussian-distribution [& {:keys [d] :or {d [640 240]}}]
  (let [[w h] d
        ctx (c/append ::gaussian-distribution d)]
    (go (while true
          (-> ctx
              (c/set-fill-style "rgba(0,0,0,0.005)")
              (c/circle [(+ (/ w 2) (* (m/rand-std-norm) 60)) (/ h 2)] 8) ; idiom
              (c/fill))
          (<! (timeout 16))))))

(defn- accept-reject []
  (let [r1 (rand) r2 (rand)]
    (if (< r2 r1) r1 (recur))))

(defn accept-reject-distribution [& {:keys [d] :or {d [640 240]}}]
  (let [[w h] d
        ctx (c/append ::accept-reject-distribution d)
        n 20
        rects (let [w' (/ w n)] (vec (for [i (range n)] [[(* i w') h] [w' h]])))]
    (go (loop [rects rects]
          (doseq [[xy [w* h*]] rects]
            (-> ctx
                (c/set-fill-style "#7F7F7F")
                (c/set-line-width 2)
                (c/begin-path)
                (c/rect xy [(dec w*) h*])
                (c/fill)
                (c/stroke)))
          (<! (timeout 16))
          (recur (update-in rects [(cljs.math/floor (* n (accept-reject))) 0 1] dec))))))

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
          (recur (m/add 0.006 t))))
    (go (while true
          (let [xy (<! in)]
            (-> ctx
                (c/begin-path)
                (c/circle xy 24)
                (c/fill)
                (c/stroke))
            (<! play))))
    ctrl))

(comment
  (def a (traditional-random-walk)) (go (>! a :toggle))
  (def b (random-distribution)) (go (>! b :toggle))
  (def c (accept-reject-distribution)) (go (>! c :toggle))
  (def d (perlin-noise-walk)) (go (>! d :toggle)))
