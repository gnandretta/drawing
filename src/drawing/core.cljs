(ns drawing.core
  (:require [drawing.canvas :as c :refer [d w] :include-macros true]
            [drawing.math :as m]
            [goog.object :as object]))

(defn drawing
  "Randomly draws at most c points inside and at most c points outside a circle
   of radius r, determined by the probabilities pi and po, respectively. fhi
   and fho are fns that take no arguments and return random hypotenuses for the
   points inside and outside the circle, fhi codomain is [0,1] and fho codomain
   is >= 1 (although large values won't fit in the canvas). bg is the color of
   the background and fgs is a map with colors as keys and weights as values,
   the colors of the points are picked from that map."
  {:dimensions [608 1080]}
  [{:keys [r c pi fhi po fho bg fgs]
    :or   {r   0.3
           c   45000
           pi  (rand)
           fhi #(- 1 (rand (rand)))
           po  (rand)
           fho #(js/Math.cosh (rand js/Math.PI))
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
      (when (<= (rand) pi)
        (c/fill-rect (m/sides a (w (fhi) r)) [1 1]))
      (when (<= (rand) po)
        (c/fill-rect (m/sides a (w (fho) r)) [1 1])))))

(defn ^:dev/after-load init []
  (c/draw drawing))
