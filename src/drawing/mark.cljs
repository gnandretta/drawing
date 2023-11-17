(ns drawing.mark
  (:require [drawing.math :as m]))

(defn squiggle
  "Returns an infinite sequence of normalized coordinates, starting with the
   cartesian equivalent of the polar coordinate (r,a), followed by coordinates
   that result of adding random numbers from the intervals dr and da to the
   previous r and a, respectively. a defaults to 0, dr and da to some intervals
   arbitrarily chosen."
  ([r] (squiggle r 0))
  ([r a] (squiggle r a [-0.01 0.01] [(m/pi 0.16) (m/pi 1.6)]))
  ([r a dr da] (lazy-seq (cons (m/sides r a)
                               (squiggle (+ r (apply m/rand-off dr))
                                         (+ a (apply m/rand-off da))
                                         dr
                                         da)))))
