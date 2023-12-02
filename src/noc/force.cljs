(ns noc.force
  (:require [cljs.core.async :refer [<! >! chan timeout] :refer-macros [alt! go]]
            [drawing.animation :as a]
            [drawing.canvas :as c :refer [j>]]
            [drawing.dom :as d]
            [drawing.math :as m]))

(defn- make-mover [& {:as attrs}]
  (merge {:xy [0 0]
          :a  [0 0]
          :v  [0 0]}
         attrs))

(defn forces [& {:keys [d fps]                              ; example 2.1
                 :or   {d   [640 240]
                        fps 60}}]
  (let [bounce (fn [[vx vy] [x y] [w h]]                    ; TODO how to model this using a force?
                 (let [[x vx] (cond (> x w) [w (* -1 vx)]
                                    (< x 0) [0 (* -1 vx)]
                                    :else [x vx])
                       [y vy] (if (> y h) [h (* -1 vy)] [y vy])]
                   [[vx vy] [x y]]))
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgba(127,127,127,0.5)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) 24)
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
    (go (loop [{:keys [a v xy] :as m} (make-mover :xy [(/ (first d) 2) 30]) m-state :up]
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
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgba(127,127,127,0.5)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) (* 8 (:mass m)))
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
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
                (j> (doseq [m ms] (draw-mover ctx m)))
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
                           (c/circle (:xy m) (- (:r m) line-width))
                           (c/fill)
                           (c/stroke)
                           (c/restore))))
        move (fn [{:keys [a v xy r mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
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
                                           (cond-> [(m/mul gravity (:mass %))]
                                                   (= m-state :down) (conj wind)))
                                    ms)
                               m-state))))
    (go (while true
          (let [ms (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (j> (doseq [m ms] (draw-mover ctx m)))
                (c/restore))
            (<! play))))
    ctrl))

;; TODO p5.js example reacts different, check params
(defn including-friction [& {:keys [d fps]                  ; example 2.4
                             :or   {d   [640 240]
                                    fps 60}}]
  (let [[w] d
        bottom? (fn [{:keys [xy r]} [_ h]] (let [[_ y] xy] (> y (- h r 1))))
        bounce (fn [[vx vy] [x y] r [w h]]
                 (let [[x vx] (cond (> x (- w r)) [(- w r) (* -1 vx)]
                                    (< x r) [r (* -1 vx)]
                                    :else [x vx])
                       [y vy] (if (> y (- h r)) [(- h r) (* -0.9 vy)] [y vy])]
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
                           (c/circle (:xy m) (- (:r m) line-width))
                           (c/fill)
                           (c/stroke)
                           (c/restore))))
        move (fn [{:keys [a v xy r mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     [v xy] (bounce v xy r d)
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        ctx (c/append ::including-friction d)
        in (chan)
        [play ctrl] (a/play fps)
        m-up-down (chan)
        gravity [0 0.1]
        wind [0.1 0]]
    (d/events (c/get ctx) "mouseup" m-up-down)
    (d/events (c/get ctx) "mousedown" m-up-down)
    (go (loop [m (make-mover :xy [(/ w 2) 30] :mass 5)
               m-state :up]
          (>! in m)
          (alt!
            m-up-down (recur m (case m-state :up :down :up))
            (timeout 1) (recur (move m (cond-> [(m/mul gravity (:mass m))]
                                               (= m-state :down) (conj wind)
                                               (bottom? m d) (conj (m/mul -0.1 (m/normalize (:v m))))))
                               m-state))))
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

(defn fluid-resistance [& {:keys [d fps]                    ; example 2.5
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
                         (c/circle (:xy m) (* 8 (:mass m)))
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     [v xy] (bounce v xy d)
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        make-liquid (fn [& {:keys [h* d c]}]
                      (let [h (* h* (second d))]
                        {:xy [0 (- (second d) h)] :d [(first d) h] :c c}))
        draw-liquid (fn [ctx {:keys [xy d]}]
                      (-> ctx
                          (c/save)
                          (c/set-fill-style "rgb(200,200,200)")
                          (c/fill-rect xy d)
                          (c/restore)))
        in? (fn [[xa ya] [xb yb] [w h]]
              (and (> xa xb) (< xa (+ xb w))
                   (> ya yb) (< yb (+ yb h))))
        calculate-liquid-resistance (fn [v c]
                                      (m/mul (m/normalize v) -1 c (m/mag-square v)))
        ctx (c/append ::fluid-resistance d)
        in (chan)
        [play ctrl] (a/play fps)
        m-down (d/events (c/get ctx) "mousedown")
        gravity [0 0.1]
        liquid (make-liquid :h* 0.5 :c 0.1 :d d)]
    (go (loop [ms nil]
          (when (nil? ms) (recur (map #(make-mover :xy [% 0] :mass (m/rand-off 0.5 3))
                                      (for [x (range 9)] (+ 40 (* 70 x))))))
          (>! in ms)
          (alt!
            m-down (recur nil)
            (timeout 1) (recur (map (fn [m] (move m (cond-> [(m/mul gravity (:mass m))]
                                                            (in? (:xy m) (:xy liquid) (:d liquid))
                                                            (conj (calculate-liquid-resistance (:v m) (:c liquid))))))
                                    ms)))))
    (go (while true
          (let [ms (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-liquid liquid)
                (j> (doseq [m ms] (draw-mover ctx m)))
                (c/restore))
            (<! play))))
    ctrl))

(defn attraction [& {:keys [d fps]                          ; example 2.6
                     :or   {d   [640 240]
                            fps 60}}]
  (let [make-mover (fn [& {:keys [xy mass] :or {mass 1}}]
                     {:xy   xy
                      :mass mass
                      :a    [0 0]
                      :v    [1 0]})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgba(127,127,127,0.5)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) (* 8 (:mass m)))
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        make-attractor (fn [& {:keys [xy mass] :or {mass 20}}]
                         {:xy xy :mass mass})
        draw-attractor (fn [ctx m]
                         (-> ctx
                             (c/save)
                             (c/set-fill-style "rgba(125,125,125,0.78)")
                             (c/set-stroke-style :black)
                             (c/set-line-width 4)
                             (c/begin-path)
                             (c/circle (:xy m) (:mass m))   ; TODO adjust numbers
                             (c/fill)
                             (c/stroke)
                             (c/restore)))
        get-attraction (fn [a b]                            ; force experienced by b due to a's attraction
                         (let [dist (m/sub (:xy a) (:xy b))
                               mag-dist (-> (m/mag dist) (max 5) (min 25))]
                           (m/mul (m/normalize dist) (/ (* (:mass a) (:mass b)) (* mag-dist mag-dist)))))
        ctx (c/append ::attraction d)
        in (chan)
        [play ctrl] (a/play fps)
        attractor (make-attractor :xy (m/div d 2))]
    (go (loop [m (make-mover :xy [350 50] :mass 2)]         ; TODO allow to move attractor
          (>! in m)
          (recur (move m [(get-attraction attractor m)]))))
    (go (while true
          (let [m (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-attractor attractor)
                (draw-mover m)
                (c/restore))
            (<! play))))
    ctrl))

(defn attraction-with-many-movers [& {:keys [d fps]         ; example 2.7
                                      :or   {d   [640 240]
                                             fps 60}}]
  (let [make-mover (fn [& {:keys [xy mass] :or {mass 1}}]
                     {:xy   xy
                      :mass mass
                      :a    [0 0]
                      :v    [1 0]})
        draw-mover (fn [ctx m]
                     (-> ctx
                         (c/save)
                         (c/set-fill-style "rgba(127,127,127,0.5)")
                         (c/set-stroke-style :black)
                         (c/set-line-width 2)
                         (c/begin-path)
                         (c/circle (:xy m) (* 8 (:mass m)))
                         (c/fill)
                         (c/stroke)
                         (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        make-attractor (fn [& {:keys [xy mass] :or {mass 20}}]
                         {:xy xy :mass mass})
        draw-attractor (fn [ctx m]
                         (-> ctx
                             (c/save)
                             (c/set-fill-style "rgba(125,125,125,0.78)")
                             (c/set-stroke-style :black)
                             (c/set-line-width 4)
                             (c/begin-path)
                             (c/circle (:xy m) (:mass m))   ; TODO adjust numbers
                             (c/fill)
                             (c/stroke)
                             (c/restore)))
        get-attraction (fn [a b]                            ; force experienced by b due to a's attraction
                         (let [dist (m/sub (:xy a) (:xy b))
                               mag-dist (-> (m/mag dist) (max 5) (min 25))]
                           (m/mul (m/normalize dist) (/ (* (:mass a) (:mass b)) (* mag-dist mag-dist)))))
        ctx (c/append ::attraction-with-many-movers d)
        in (chan)
        [play ctrl] (a/play fps)
        attractor (make-attractor :xy (m/div d 2))]
    (go (loop [ms (repeatedly 10 #(make-mover :xy (mapv rand d) :mass (m/rand-off 0.5 3)))] ; TODO allow to move attractor
          (>! in ms)
          (recur (map #(move % [(get-attraction attractor %)]) ms))))
    (go (while true
          (let [ms (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (draw-attractor attractor)
                (j> (doseq [m ms] (draw-mover ctx m)))
                (c/restore))
            (<! play))))
    ctrl))

(defn two-bodies-attraction [& {:keys [d fps]               ; example 2.8
                                :or   {d   [640 240]
                                       fps 60}}]
  (let [ctx (c/append ::two-bodies-attraction d)
        in (chan)
        [play ctrl] (a/play fps)
        make-body (fn [& {:keys [xy v mass] :or {v    [1 0]
                                                 mass 8}}]
                    {:xy   xy
                     :mass mass
                     :a    [0 0]
                     :v    v})
        draw-body (fn [ctx m]
                    (-> ctx
                        (c/save)
                        (c/set-fill-style "rgba(127,127,127,0.5)")
                        (c/set-stroke-style :black)
                        (c/set-line-width 2)
                        (c/begin-path)
                        (c/circle (:xy m) (* 4 (js/Math.sqrt (:mass m)))) ; TODO adjust numbers
                        (c/fill)
                        (c/stroke)
                        (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        get-attraction (fn [a b]                            ; force experienced by b due to a's attraction
                         (let [dist (m/sub (:xy a) (:xy b))
                               mag-dist (-> (m/mag dist) (max 5) (min 25))]
                           (m/mul (m/normalize dist) (/ (* (:mass a) (:mass b)) (* mag-dist mag-dist)))))
        body-a (make-body :xy [320 40] :v [1 0])
        body-b (make-body :xy [320 200] :v [-1 0])]
    (go (loop [body-a body-a body-b body-b]
          (>! in [body-a body-b])
          (recur (move body-a [(get-attraction body-b body-a)])
                 (move body-b [(get-attraction body-a body-b)]))))
    (go (while true
          (let [bodies (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (j> (doseq [m bodies] (draw-body ctx m)))
                (c/restore))
            (<! play))))
    ctrl))

(defn n-bodies [& {:keys [d fps]                            ; example 2.9
                   :or   {d   [640 240]
                          fps 60}}]
  (let [ctx (c/append ::n-bodies d)
        in (chan)
        [play ctrl] (a/play fps)
        make-body (fn [& {:keys [xy v mass] :or {v    [0 0]
                                                 mass 8}}]
                    {:xy   xy
                     :mass mass
                     :a    [0 0]
                     :v    v})
        draw-body (fn [ctx m]
                    (-> ctx
                        (c/save)
                        (c/set-fill-style "rgba(127,127,127,0.5)")
                        (c/set-stroke-style :black)
                        (c/set-line-width 2)
                        (c/begin-path)
                        (c/circle (:xy m) (* 8 (:mass m)))  ; TODO adjust numbers
                        (c/fill)
                        (c/stroke)
                        (c/restore)))
        move (fn [{:keys [a v xy mass] :as m} forces]
               (let [a (apply mapv + a (map #(m/div % mass) forces)) ; TODO a always is [0 0] because is reset at the end, seems counter intuitive
                     v (mapv + v a)
                     xy (mapv + xy v)]
                 (merge m {:xy xy :v v :a [0 0]})))
        get-attraction (fn [a b]                            ; force experienced by b due to a's attraction
                         (let [dist (m/sub (:xy a) (:xy b))
                               mag-dist (-> (m/mag dist) (max 5) (min 25))]
                           (m/mul (m/normalize dist) (/ (* (:mass a) (:mass b)) (* mag-dist mag-dist)))))
        ]
    (go (loop [bodies (repeatedly 10 #(make-body :xy (mapv rand-int d) :mass (m/rand-off 0.1 2)))]
          (>! in bodies)
          (recur (map (fn [a]
                        (let [forces (->> bodies (filter (partial not= a))
                                          (map (fn [b] (get-attraction b a))))]
                          (move a forces)))
                      bodies))))
    (go (while true
          (let [bodies (<! in)]
            (-> ctx
                (c/save)
                (c/set-fill-style :white)
                (c/fill-rect d)
                (j> (doseq [m bodies] (draw-body ctx m)))
                (c/restore))
            (<! play))))
    ctrl))

(comment
  (def a (forces)) (go (>! a :toggle))
  (def b (forces-acting-on-two-objects)) (go (>! b :toggle))
  (def c (gravity-scaled-by-mass)) (go (>! c :toggle))
  (def d (including-friction)) (go (>! d :toggle))
  (def e (fluid-resistance)) (go (>! e :toggle))
  (def f (attraction)) (go (>! f :toggle))
  (def g (attraction-with-many-movers)) (go (>! g :toggle))
  (def h (two-bodies-attraction)) (go (>! h :toggle))
  (def i (n-bodies)) (go (>! i :toggle)))
