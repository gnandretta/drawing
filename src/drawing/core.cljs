(ns drawing.core
  (:require [drawing.canvas :as c :refer [d w] :include-macros true]
            [drawing.math :as m]
            [goog.object :as object]))

(defn drawing
  "Randomly draws c points inside or outside a circle of radius r on a
   background bg. The colors of the points are picked from the fgs map, which
   has colors as keys and weights as values. The position of the point is
   determined by the hypotenuse function fh which takes no arguments and returns
   a number—[0,1] inside the circle, and >= 1 outside (which may not fit in the
   canvas)."
  {:size [608 1080]
   :margin [40 40 40 40]}
  [{:keys [r c fh bg fgs]
    :or   {r   0.3
           c   45000
           fh #(if (< (rand) 0.5)
                 (- 1 (rand (rand)))
                 (js/Math.cosh (rand js/Math.PI)))
           bg  "rgb(229,228,228)"
           fgs {"rgb(109,79,246)"  4
                "rgb(64,0,131)"    2
                "rgb(57,0,3)"      2
                "rgb(255,195,190)" 2
                "rgb(255,255,255)" 1}}}]
  (c/set-fill-style bg)
  (c/fill-rect [0 0] (d))
  (c/translate (d 0.5))
  (doseq [_ (range c)]
    (let [a (rand (* 2 js/Math.PI))]
      (c/set-fill-style (m/weighed-rand-key fgs))
      (c/fill-rect (m/sides a (w (fh) r)) [1 1]))))

(defn ^:dev/after-load init []
  (c/draw drawing))
