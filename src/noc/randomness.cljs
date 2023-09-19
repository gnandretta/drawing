(ns noc.randomness
  (:require [cljs.core.async :as a :refer [<!] :refer-macros [go-loop]]
            [drawing.canvas :as c]))

(defn traditional-random-walk [& {:keys [size fps]
                                  :or   {size [640 420]
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
        ctx (c/ctx (c/create "random-walker" size))]
    (-> ctx
        (c/set-fill-style "#fff")
        (c/fill-rect [0 0] size))
    (go-loop [p (mapv (partial * 0.5) size)]
      (-> ctx
          (c/set-fill-style "#000")
          (c/fill-rect p [1 1]))
      (<! (a/timeout (/ 1000 fps)))
      (recur (step p)))))
