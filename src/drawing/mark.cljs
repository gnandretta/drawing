(ns drawing.mark
  (:require [drawing.math :as m]))

(defn squiggle                                              ; TODO there are not normalized coordinates, remove defaults
  "Returns an infinite sequence of normalized coordinates, starting with the
   cartesian equivalent of the polar coordinate (r,a), followed by coordinates
   that result of adding random numbers from the intervals dr and da to the
   previous r and a, respectively. a defaults to 0, dr and da to some intervals
   arbitrarily chosen."
  ([r] (squiggle r 0))
  ([r a] (squiggle r a [-0.01 0.01] [(m/pi 0.16) (m/pi 1.6)]))
  ([r a dr da]
   (map m/xy (iterate #(m/add % (map m/rand-off [dr da]))
                      [r a]))))

(defn variably-thick-line                                   ; TODO n = total hatch marks? keep iterating, add docs when happy
  [n w moments]
  (mapcat (fn [[xa aa] [xb ab]]
            (when-not (= xa xb)
              (for [i (range 0 n) :let [x (m/lerp i [0 (dec n)] [xa xb])
                                        a (m/lerp i [0 (dec n)] [aa ab])
                                        start [x 0]]]
                [start (m/add start (m/xy w a))])))
          moments
          (rest moments)))
