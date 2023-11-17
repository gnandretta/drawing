(ns mdn.canvas-api-tutorial.compositing-and-clipping
  (:require [drawing.canvas :as c]
            [drawing.cljs :refer [jump->]]
            [drawing.math :as m]))

(defn a-clip-example []                                     ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Compositing#a_clip_example
  (let [draw-star (fn [ctx r]
                    (-> ctx
                        (c/save)
                        (c/begin-path)
                        (c/move-to [r 0])
                        (jump->
                          (doseq [i (range 9) :let [x (cond-> r (even? i) (* 0.200811 (/ 1 0.525731)))]]
                            (-> ctx
                                (c/rotate (m/pif 5))
                                (c/line-to [x 0]))))
                        (c/close-path)
                        (c/fill)
                        (c/restore)))
        ctx (c/append ::a-clip-example [150 150])]
    (-> ctx
        (c/fill-rect [150 150])
        (c/translate [75 75])
        (c/begin-path)
        (c/circle 60)
        (c/clip)
        (c/set-fill-style (c/linear-gradient ctx [0 -75] [0 75]
                                             [[0 "#232256"]
                                              [1 "#143778"]]))
        (c/fill-rect [-75 -75] [150 150]))
    (doseq [j (range 50)]
      (-> ctx
          (c/save)
          (c/set-fill-style :white)
          (c/translate (repeatedly 2 #(m/rand-int-off -75 75)))
          (draw-star (m/rand-int-off 2 6))
          (c/restore)))))

(comment
  (a-clip-example))
