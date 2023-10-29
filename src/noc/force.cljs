(ns noc.force
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [alt! go]]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.cljs :refer [jump->]]
            [drawing.dom :as d]
            [drawing.math :as m]))

(defn forces [& {:keys [d fps]                              ; example 2.1
                 :or   {d   [640 240]
                        fps 60}}]
  (let [bounce (fn [[vx vy] [x y] [w h]]                    ; TODO how to model this using a force?
                 (let [[x vx] (cond (> x w) [w (* -1 vx)]
                                    (< x 0) [0 (* -1 vx)]
                                    :else [x vx])
                       [y vy] (if (> y h) [h (* -1 vy)] [y vy])]
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

(defn forces-acting-on-two-objects [& {:keys [d fps]        ; example 2.2
                                       :or   {d   [640 240]
                                              fps 60}}]
  (let [bounce (fn [[vx vy] [x y] [w h]]
                 (let [[x vx] (cond (> x w) [w (* -1 vx)]
                                    (< x 0) [0 (* -1 vx)]
                                    :else [x vx])
                       [y vy] (if (> y h) [h (* -1 vy)] [y vy])]
                   [[vx vy] [x y]]))
        make-mover (fn [& {:keys [xy mass] :or {mass 1}}]
                     {:xy   xy
                      :mass mass
                      :a    [0 0]
                      :v    [0 0]})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgba(127,127,127,0.5)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/arc (:xy m) (* 8 (:mass m)) 0 (m/pi 2))
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/v-div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     [v xy] (bounce v xy d)
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        ctx (c/append ::forces-acting-on-two-objects d)
        in (chan)
        [play ctrl] (a/play fps)
        m-up-down (chan)
        gravity [0 0.1]
        wind [0.1 0]]
    (d/events (c/get ctx) "mouseup" m-up-down)
    (d/events (c/get ctx) "mousedown" m-up-down)
    (go (loop [ms [(make-mover :xy [200 30] :mass 10)
                   (make-mover :xy [440 30] :mass 2)]
               m-state :up]
          (>! in ms)
          (alt!
            m-up-down (recur ms (case m-state :up :down :up))
            (timeout 1) (let [forces (cond-> [gravity] (= m-state :down) (conj wind))]
                          (recur (map #(move % forces) ms) m-state)))))
    (go (while true
          (let [ms (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (jump-> (doseq [m ms] (draw-mover ctx m)))
                (c/restore))
            (<! play))))
    ctrl))

(defn gravity-scaled-by-mass [& {:keys [d fps]              ; example 2.3
                                 :or   {d   [640 240]
                                        fps 60}}]
  (let [bounce (fn [[vx vy] [x y] r [w h]]
                 (let [[x vx] (cond (> x (- w r)) [(- w r) (* -1 vx)]
                                    (< x r) [r (* -1 vx)]
                                    :else [x vx])
                       [y vy] (if (> y (- h r)) [(- h r) (* -1 vy)] [y vy])]
                   [[vx vy] [x y]]))
        make-mover (fn [& {:keys [xy mass] :or {mass 1}}]
                     {:xy   xy
                      :mass mass
                      :r    (* 8 mass)
                      :a    [0 0]
                      :v    [0 0]})
        draw-mover (fn [ctx m]
                     (let [line-width 2]
                       (-> ctx
                           (c/save)
                           (c/set-fill-style "rgba(127,127,127,0.5)")
                           (c/set-stroke-style :black)
                           (c/set-line-width line-width)
                           (c/begin-path)
                           (c/arc (:xy m) (- (:r m) line-width) 0 (m/pi 2))
                           (c/fill)
                           (c/stroke)
                           (c/restore))))
        move (fn [{:keys [a v xy r mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/v-div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     [v xy] (bounce v xy r d)
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        ctx (c/append ::gravity-scaled-by-mass d)
        in (chan)
        [play ctrl] (a/play fps)
        m-up-down (chan)
        gravity [0 0.1]
        wind [0.1 0]]
    (d/events (c/get ctx) "mouseup" m-up-down)
    (d/events (c/get ctx) "mousedown" m-up-down)
    (go (loop [ms [(make-mover :xy [200 30] :mass 10)
                   (make-mover :xy [440 30] :mass 2)]
               m-state :up]
          (>! in ms)
          (alt!
            m-up-down (recur ms (case m-state :up :down :up))
            (timeout 1) (recur (map #(move %
                                           (cond-> [(m/v* gravity (:mass %))]
                                                   (= m-state :down) (conj wind)))
                                    ms)
                               m-state))))
    (go (while true
          (let [ms (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (jump-> (doseq [m ms] (draw-mover ctx m)))
                (c/restore))
            (<! play))))
    ctrl))
