(ns drawing.core
  (:require [drawing.canvas :as c :include-macros true]
            [drawing.math :as m]
            [goog.object :as object]))

(defn drawing
  {:dimensions [608 1080]}
  [{:keys [d w]}]
  (let [r 0.3
        pi (rand)
        po (rand)]
    (c/set-fill-style "rgb(229,228,228)")
    (c/fill-rect [0 0] (d))
    (c/translate (d 0.5))
    (doseq [_ (range 45000)]
      (let [a (rand (* 2 js/Math.PI))]
        (c/set-fill-style (m/weighed-rand-key {"rgb(109,79,246)"  4
                                               "rgb(64,0,131)"    2
                                               "rgb(57,0,3)"      2
                                               "rgb(255,195,190)" 2
                                               "rgb(255,255,255)" 1}))
        (when (<= (rand) pi)
          (c/fill-rect (m/sides a (w (- 1 (rand (rand))) r))
                       [1 1]))
        (when (<= (rand) po)
          (c/fill-rect (m/sides a (w (js/Math.cosh (rand js/Math.PI)) r))
                       [1 1]))))))

(defn ^:dev/after-load init []
  (c/draw drawing))
