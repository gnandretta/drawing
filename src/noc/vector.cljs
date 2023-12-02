(ns noc.vector
  (:require [cljs.core.async :as async :refer [<! >! chan timeout] :refer-macros [alt! go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.dom :as d]
            [drawing.math :as m]))

(defn bouncing-ball-with-no-vectors [& {:keys [d fps]
                                        :or   {d   [640 240]
                                               fps 30}}]
  (let [[w h] d
        in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::bouncing-ball-with-no-vectors d)]
    (go (loop [x 100 y 100 vx 2.5 vy 2]
          (let [x (+ x vx)
                y (+ y vy)]
            (>! in {:x x :y y})
            (recur x
                   y
                   (m/sign vx (m/in? x [0 w]))
                   (m/sign vy (m/in? y [0 h]))))))
    (go (while true
          (let [{:keys [x y]} (<! in)]
            (-> ctx
                (c/set-fill-style :white)
                (c/fill-rect d)
                (c/set-fill-style "rgb(127,127,127)")
                (c/set-line-width 2)
                (c/begin-path)
                (c/circle [x y] 48)
                (c/fill)
                (c/stroke))
            (<! play))))
    ctrl))

(defn bouncing-ball-with-vectors [& {:keys [d fps]
                                     :or   {d   [640 240]
                                            fps 30}}]
  (let [[w h] d
        in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::bouncing-ball-with-vectors d)]
    (go (loop [xy [100 100] v [2.5 2]]
          (let [xy (mapv + xy v)
                v (mapv m/sign v (mapv m/in? xy [[0 w] [0 h]]))]
            (>! in xy)
            (recur xy v))))
    (go (while true
          (let [xy (<! in)]
            (-> ctx
                (c/set-fill-style :white)
                (c/fill-rect d)
                (c/set-fill-style "rgb(127,127,127)")
                (c/set-line-width 2)
                (c/begin-path)
                (c/circle xy 48)
                (c/fill)
                (c/stroke))
            (<! play))))
    ctrl))

(defn vector-subtraction [& {:keys [d fps]
                             :or   {d   [640 240]
                                    fps 60}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::vector-subtraction d)
        mouse (async/map (fn [e] [(.-offsetX e) (.-offsetY e)])
                         [(d/events (c/get ctx) "mousemove" (chan (async/sliding-buffer 1)))])
        center (mapv (partial * 0.5) d)]
    (go (>! in center)
        (while true
          (>! in (<! mouse))
          (<! (timeout 1))))
    (go (while true
          (let [xy (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (c/set-line-width 4)
                (c/set-stroke-style "rgb(200,200,200)")
                (c/begin-path)
                (c/move-to [0 0])                           ; from origin (top-left)
                (c/line-to xy)                              ; to mouse
                (c/move-to [0 0])                           ; from origin (top-left)
                (c/line-to center)                          ; to center
                (c/stroke)
                (c/set-stroke-style :black)
                (c/translate center)                        ; origin=center, to visualize the difference vector—from center to mouse
                (c/begin-path)
                (c/move-to [0 0])                           ; from origin (center)
                (c/line-to (mapv - xy center))              ; to difference of mouse and center
                (c/stroke)
                (c/restore))
            (<! play))))
    ctrl))

(defn vector-multiplication [& {:keys [d fps]
                                :or   {d   [640 240]
                                       fps 60}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::vector-multiplication d)
        mouse (async/map (fn [e] [(.-offsetX e) (.-offsetY e)])
                         [(d/events (c/get ctx) "mousemove" (chan (async/sliding-buffer 1)))])
        center (mapv (partial * 0.5) d)]
    (go (>! in center)
        (while true
          (>! in (<! mouse))
          (<! (timeout 1))))
    (go (while true
          (let [xy (mapv - (<! in) center)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (c/translate center)
                (c/set-stroke-style "rgb(200,200,200)")
                (c/set-line-width 2)
                (c/begin-path)
                (c/move-to [0 0])
                (c/line-to xy)
                (c/stroke)
                (c/set-stroke-style :black)
                (c/set-line-width 2)
                (c/begin-path)
                (c/move-to [0 0])
                (c/line-to (mapv (partial * 0.5) xy))
                (c/stroke)
                (c/restore))
            (<! play))))
    ctrl))

(defn vector-magnitude [& {:keys [d fps]
                           :or   {d   [640 240]
                                  fps 60}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::vector-magnitude d)
        mouse (async/map (fn [e] [(.-offsetX e) (.-offsetY e)])
                         [(d/events (c/get ctx) "mousemove" (chan (async/sliding-buffer 1)))])
        center (mapv (partial * 0.5) d)]
    (go (>! in center)
        (while true
          (>! in (<! mouse))
          (<! (timeout 1))))
    (go (while true
          (let [xy (mapv - (<! in) center)
                mag-xy (m/mag xy)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (c/set-fill-style :black)
                (c/fill-rect [10 10] [mag-xy 10])
                (c/translate center)
                (c/begin-path)
                (c/move-to [0 0])
                (c/line-to xy)
                (c/stroke)
                (c/restore))
            (<! play))))
    ctrl))


(defn vector-normalize [& {:keys [d fps]
                           :or   {d   [640 240]
                                  fps 60}}]
  (let [in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::vector-normalize d)
        mouse (async/map (fn [e] [(.-offsetX e) (.-offsetY e)])
                         [(d/events (c/get ctx) "mousemove" (chan (async/sliding-buffer 1)))])
        center (mapv (partial * 0.5) d)]
    (go (>! in center)
        (while true
          (>! in (<! mouse))
          (<! (timeout 1))))
    (go (while true
          (let [xy (mapv - (<! in) center)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (c/translate center)
                (c/set-stroke-style "rgb(200,200,200)")
                (c/set-line-width 2)
                (c/begin-path)
                (c/move-to [0 0])
                (c/line-to xy)
                (c/stroke)
                (c/set-stroke-style :black)
                (c/set-line-cap :square)
                (c/set-line-width 8)
                (c/begin-path)
                (c/move-to [0 0])
                (c/line-to (mapv (partial * 50) (m/normalize xy)))
                (c/stroke)
                (c/restore))
            (<! play))))
    ctrl))

(defn- teleport [xy d]
  (mapv (fn [n b] (cond (< n 0) b
                        (> n b) 0
                        :else n))
        xy
        d))

(defn motion-101-velocity [& {:keys [d fps]
                              :or   {d   [640 240]
                                     fps 60}}]
  (let [make-mover (fn [] {:xy (mapv rand d)
                           :v  (into [] (repeatedly 2 #(m/rand-off -2 2)))})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgb(127,127,127)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) 48)
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        move (fn [xy v] (mapv + xy v))
        in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::motion-101-velocity d)]
    (go (loop [m (make-mover)]
          (>! in m)
          (recur (-> m
                     (update :xy move (:v m))
                     (update :xy teleport d)))))
    (go (while true
          (let [m (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-mover m)
                (c/restore))
            (<! play))))
    ctrl))

(defn motion-101-velocity-and-constant-acceleration [& {:keys [d fps]
                                                        :or   {d   [640 240]
                                                               fps 60}}]
  (let [make-mover (fn [] {:xy (mapv rand d)
                           :a  [-0.001 0.01]
                           :v  (into [] (repeatedly 2 #(m/rand-off -2 2)))})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgb(127,127,127)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) 48)
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        limit (fn [v n]                                     ; TODO extract fn, probably more efficient to use mag^2
                (let [mag-v (m/mag v)]
                  (cond-> v (> mag-v n) (m/div mag-v))))
        in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::motion-101-velocity-and-constant-acceleration d)]
    (go (loop [{:keys [v xy] :as m} (make-mover)]
          (>! in m)
          (let [v (limit (m/add v (:a m)) 10)
                xy (teleport (m/add xy v) d)]
            (recur (merge m {:v v :xy xy})))))
    (go (while true
          (let [m (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-mover m)
                (c/restore))
            (<! play))))
    ctrl))

(defn motion-101-velocity-and-random-acceleration [& {:keys [d fps]
                                                      :or   {d   [640 240]
                                                             fps 60}}]
  (let [rand-v (fn [] (into [] (repeatedly 2 #(m/rand-off -2 2))))
        make-mover (fn [] {:xy (mapv rand d)
                           :a  (rand-v)
                           :v  (rand-v)})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgb(127,127,127)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) 48)
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        limit (fn [v n]
                (let [mag-v (m/mag v)]
                  (cond-> v (> mag-v n) (m/div mag-v))))
        in (chan)
        [play ctrl] (a/play fps)
        ctx (c/append ::motion-101-velocity-and-random-acceleration d)]
    (go (loop [{:keys [v xy] :as m} (make-mover)]
          (>! in m)
          (let [a (rand-v)
                v (limit (m/add v a) 10)
                xy (teleport (m/add xy v) d)]
            (recur (merge m {:v v :xy xy})))))
    (go (while true
          (let [m (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-mover m)
                (c/restore))
            (<! play))))
    ctrl))

(defn accelerating-towards-the-mouse [& {:keys [d fps]
                                         :or   {d   [640 240]
                                                fps 60}}]
  (let [rand-v (fn [] (into [] (repeatedly 2 #(m/rand-off -2 2))))
        make-mover (fn [] {:xy (mapv rand d)
                           :a  (rand-v)
                           :v  (rand-v)})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgb(127,127,127)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) 48)
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        limit (fn [v n]
                (let [mag-v (m/mag v)]
                  (cond-> v (> mag-v n) (m/div mag-v))))
        ctx (c/append ::accelerating-towards-the-mouse d)
        in (chan)
        [play ctrl] (a/play fps)
        mouse (async/map (fn [e] [(.-offsetX e) (.-offsetY e)])
                         [(d/events (c/get ctx) "mousemove" (chan (async/sliding-buffer 1)))])]
    (go (loop [{:keys [v xy] :as m} (make-mover) m-xy [0 0]]
          (>! in m)
          (let [m-xy (alt! mouse ([m-xy] m-xy)
                           (timeout 10) m-xy)
                a (m/mul 0.2 (m/normalize (mapv - m-xy xy)))
                v (limit (m/add v a) 10)
                xy (m/add xy v)]
            (recur (merge m {:v v :xy xy}) m-xy))))
    (go (while true
          (let [m (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-mover m)
                (c/restore))
            (<! play))))
    ctrl))

(comment
  (def a (bouncing-ball-with-no-vectors)) (go (>! a :toggle))
  (def b (bouncing-ball-with-vectors)) (go (>! b :toggle))
  (def c (vector-subtraction)) (go (>! c :toggle))
  (def d (vector-multiplication)) (go (>! c :toggle))
  (def e (vector-magnitude)) (go (>! e :toggle))
  (def f (vector-normalize)) (go (>! f :toggle))
  (def g (motion-101-velocity)) (go (>! g :toggle))
  (def h (motion-101-velocity-and-constant-acceleration)) (go (>! h :toggle))
  (def i (motion-101-velocity-and-random-acceleration)) (go (>! i :toggle))
  (def j (accelerating-towards-the-mouse)) (go (>! j :toggle)))
