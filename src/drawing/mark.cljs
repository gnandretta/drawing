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

(defn variably-thick-line                                   ; TODO n = total hatch marks? keep iterating, follow a path (another fn?), add docs when happy
  [n w moments]
  (mapcat (fn [[xa aa] [xb ab]]
            (when-not (= xa xb)
              (for [i (range 0 n) :let [x (m/lerp i [0 (dec n)] [xa xb])
                                        a (m/lerp i [0 (dec n)] [aa ab])
                                        start [x 0]]]
                [start (m/add start (m/xy w a))])))
          moments
          (rest moments)))

(defn drunken-ant-path                                      ; TODO rename to random walk or something like that and review doc
  "Returns an infinite sequence of points that begins with xy and each
   successive point is the result of applying some random acceleration
   (magnified by drinks) and some random angular acceleration."
  [xy drinks]
  (let [a-mags (iterate (fn [a] (+ a (* (m/rand-off -0.1 0.1) drinks))) 0)
        aa-mags (rest (iterate (fn [aa] (+ aa (m/rand-off -1 1))) 0))
        acc (fn [v [a aa]] (m/mag (m/rotate v aa)
                                  (+ (m/mag v) a)))]
    (reductions m/add xy (reductions acc
                                     (m/normalize [(rand) (rand)])
                                     (map vector a-mags aa-mags)))))

(defn drifting-dash-polyline
  "Returns a sequence of pairs of line end points that go through xys, have
   length l-line, (mostly) separated by gaps of length l-gap and randomly moved
   a little bit n-drift times. If trace? is true, the movements are added to
   the sequence along with the original lines."
  [xys [l-line l-gap] n-drifts trace?]
  (->> (mapcat (fn [a b]                                    ; make dashed lines between neighbour xys
                 (let [dir (m/sub b a)
                       l (+ l-line l-gap)]
                   (->> (iterate #(m/add % (m/mag dir l)) a)
                        (take-while #(>= (m/dist % b) l))
                        (map #(vector % (m/add % (m/mag dir l-line)))))))
               xys
               (rest xys))
       (iterate (fn [xy-pairs]                              ; move lines a little bit or make them
                  (cond->> (map #(mapv (partial m/add %2) %1)
                                xy-pairs
                                (partition 2 (repeatedly #(m/rand-off -10 10)))) ; random direction *vectors*
                           trace? (interleave xy-pairs))))
       (drop n-drifts)
       (first)))

(defn grid
  "Returns a lazy sequence of pairs of line end points that from an r x c grid
   of size w x h."
  [r c [w h]]
  (concat (for [y (m/rpart (inc r) w)]
            [[0 y] [w y]])
          (for [x (m/rpart (inc c) h)]
            [[x 0] [x h]])))
