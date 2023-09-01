(ns drawing.core
  (:require [drawing.canvas :as c :include-macros true]
            [drawing.math :as m]
            [goog.object :as object]))

(defn drawing
  {:width  608
   :height 1080}
  [{:keys [width height]}]
  (let [circle-radius (* width 0.3)
        probability-inside (rand)
        probability-outside (rand)]
    (c/set-fill-style "rgb(229,228,228)")
    (c/fill-rect 0 0 width height)
    (c/translate (/ width 2) (/ height 2))
    (doseq [_ (range 45000)]
      (let [angle (rand (* 2 js/Math.PI))
            hypotenuse-inside (* (- 1 (rand (rand))) circle-radius)
            hypotenuse-outside (* (js/Math.cosh (rand js/Math.PI)) circle-radius)]
        (c/set-fill-style (m/weighed-rand-key {"rgb(109,79,246)"  4
                                               "rgb(64,0,131)"    2
                                               "rgb(57,0,3)"      2
                                               "rgb(255,195,190)" 2
                                               "rgb(255,255,255)" 1}))
        (when (<= (rand) probability-inside)
          (c/fill-rect (* hypotenuse-inside (js/Math.cos angle))
                       (* hypotenuse-inside (js/Math.sin angle))
                       1
                       1))
        (when (<= (rand) probability-outside)
          (c/fill-rect (* hypotenuse-outside (js/Math.cos angle))
                       (* hypotenuse-outside (js/Math.sin angle))
                       1
                       1))))))

(defn ^:dev/after-load init []
  (c/draw drawing))
