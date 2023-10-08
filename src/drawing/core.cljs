(ns drawing.core
  (:require [drawing.canvas :as c]
            [drawing.math :as m]))

(defn drawing
  "Randomly draws c points inside or outside a circle of radius r on a
   background bg. The colors of the points are picked from the fgs map, which
   has colors as keys, and weights as values. The position of the point is
   determined by the hypotenuse functions fhi and fho which take no arguments
   and (must) return a number between [0,1] (inside the circle) and >= 1
   (outside), respectively. The probability of using fhi for a point is p, and
   the probability of using fho is 1-p. Note that two or more points may be
   drawn in the same coordinate and that some points may not fit the canvas
   (when fho returns a number large enough)."
  [& {:keys [id size r c p fhi fho bg fgs]
      :or   {id  ::drawing
             r   0.3
             c   45000
             p   0.7
             fhi #(- 1 (rand (rand)))
             fho #(js/Math.cosh (rand js/Math.PI))
             bg  "rgb(229,228,228)"
             fgs {"rgb(109,79,246)"  4
                  "rgb(64,0,131)"    2
                  "rgb(57,0,3)"      2
                  "rgb(255,195,190)" 2
                  "rgb(255,255,255)" 1}}}]
  (let [ctx (c/append id size)
        {:keys [d w]} (c/layout :size size)]
    (-> ctx
        (c/set-fill-style bg)
        (c/fill-rect [0 0] (d))
        (c/translate (d 0.5)))
    (doseq [_ (range c)]
      (let [a (rand (* 2 js/Math.PI))]
        (-> ctx
            (c/set-fill-style (m/weighed-rand-key fgs))
            (c/fill-rect (m/sides a (w ((if (< (rand) p) fhi fho)) r)) [1 1]))))))

(defn ^:dev/after-load init []
  (drawing :size [608 1080]))
