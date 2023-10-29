(ns noc.force
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [alt! go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.dom :as d]
            [drawing.math :as m]))

(defn forces [& {:keys [d fps]                              ; example 2.1
                 :or   {d   [640 240]
                        fps 60}}]
  (let [bounce (fn [[vx vy] [x y] [w h]]                    ; TODO how to model this using a force?
                 (let [[x vx] (cond (> x w) [w (* -1 vx)]
                                    (< x 0) [0 (* -1 vx)]
                                    :else [x vx])
                       [y vy] (if (> y h) [y (* -1 vy)] [y vy])]
                   [[vx vy] [x y]]))
        make-mover (fn [] {:xy [(/ (first d) 2) 30]
                           :a  [0 0]
                           :v  [0 0]})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgba(127,127,127,0.5)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/arc (:xy m) 24 0 (m/pi 2))
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        ctx (c/append ::forces d)
        in (chan)
        [play ctrl] (a/play fps)
        m-up-down (chan)
        gravity [0 0.1]
        wind [0.1 0]]
    (d/events (c/get ctx) "mouseup" m-up-down)
    (d/events (c/get ctx) "mousedown" m-up-down)
    (go (loop [{:keys [a v xy] :as m} (make-mover) m-state :up]
          (>! in m)
          (alt!
            m-up-down (recur m (case m-state :up :down :up))
            (timeout 1) (let [a (mapv + a gravity (case m-state :down wind [0 0]))
                              [v xy] (bounce v xy d)
                              v (mapv + v a)
                              xy (mapv + xy v)]
                          (recur (merge {:xy xy :v v :a [0 0]}) m-state)))))
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
