(ns drawing.math
  (:require [cljs.math :as m]
            ["random$default" :as random]
            ["simplex-noise" :refer [createNoise2D]]))

(defn- op [f xs]
  (let [parse (fn [x] (cond (fn? x) (repeatedly x)
                            (seqable? x) x
                            :else (repeat x)))]
    (reduce (fn [acc x] (mapv f acc (parse x)))
            (parse (first xs))
            (rest xs))))

(defn add
  [& xs]
  (op + xs))

(defn sub
  [& xs]
  (op - xs))

(defn mul
  [& xs]
  (op * xs))

(defn div
  [& xs]
  (op / xs))

(def v+ add)                                                ; TODO remove v* fns after updating all drawings
(def v- sub)
(def v* mul)
(def v-div div)

(defn lerp
  "Maps n from [a,b] (defaults to [0,1]) to [c,d] with a linear interpolation."
  ([n [c d]] (lerp n [0 1] [c d]))                          ; also works when d < c
  ([n [a b] [c d]] (+ (* (- d c)
                         (/ (- n a) (- b a)))
                      c)))

(defn lerpv
  "Returns a vector with the mappings of n from [0,1] to [ax,bx] and [ay,by]."
  [n a b]
  (mapv #(lerp n %) (map vector a b)))

(defn dist
  "Returns the Euclidean distance between (x0,y0) and (x1, y1)."
  [[x0 y0] [x1 y1]]
  (let [dx (- x1 x0) dy (- y1 y0)]
    (m/sqrt (+ (* dx dx) (* dy dy)))))

(defn adj
  "Given the length of the hypotenuse of a right triangle and its acute angle,
   calculates the length of its adjacent side."
  [h a]
  (* (m/cos a) h))

(defn opp
  "Given the length of the hypotenuse of a right triangle and its acute angle,
   calculates the length of its opposite side."
  [h a]
  (* (m/sin a) h))

(defn coord
  "Converts a polar coordinate to a Cartesian coordinate."
  ([[r a]] (coord r a))
  ([r a] [(adj r a) (opp r a)]))

(defn rad
  "Converts degrees to radians."
  [deg]
  (* deg (/ m/PI 180)))

(defn deg
  "Converts radians to degrees."
  [rad]
  (* rad (/ 180 m/PI)))

(defn pi
  "Multiplies PI by the given numbers."
  [& ns]
  (apply * m/PI ns))

(defn pif
  "Divides PI by the given numbers, computing a fraction."
  [& ns]
  (apply / m/PI ns))

(defn pir
  "Returns an interval of multiples of PI"
  ([[a b]] (pir a b))
  ([a b] (mapv pi [a b])))

(defn weighed-rand-key
  "Given a map where the values are weights, returns a random key based on the
   weights."
  [m]
  (let [interval-ends (reductions + (vals m))
        n (rand (last interval-ends))]
    (loop [[[e k] & eks] (map vector interval-ends (keys m))]
      (when e
        (if (< n e)
          k
          (recur eks))))))

(defn rand-int-off
  "Returns a random integer between a (inclusive) and b (exclusive)."
  ([[a b]] (rand-int-off a b))
  ([a b] (+ a (rand-int (- b a)))))

(defn rand-off
  "Returns a random floating point number between a (inclusive) and b
   (exclusive)."
  ([[a b]] (rand-off a b))
  ([a b] (+ a (rand (- b a)))))

(defonce rand-std-norm-generator (.normal random))          ; TODO allow to set (and get?) a seed

(defn rand-std-norm
  "Returns a random floating point number sampled from a standard (μ=0, σ=1)
   normal distribution. Use (+ (* σ (rand-std-norm) μ) to obtain a sample from
   another normal distribution."
  []
  (rand-std-norm-generator))

(defonce noise-2d-generator (createNoise2D))                ; TODO see alea npm module

(defn noise-1d
  "Returns a simplex noise value (in the range [-1,1]) at the specified
   coordinate."
  [x]
  (noise-2d-generator x 0))

(defn noise-2d
  "Returns a simplex noise value (in the range [-1,1]) at the specified
   coordinate."
  [[x y]]
  (noise-2d-generator x y))

(defn range-n
  "Returns a lazy sequence of start, start+step, start+2*step, ...,
   start+n*step."
  [start n step]
  (take n (iterate (partial + step) start)))

(defn grid
  "Returns a lazy sequence of normalized coordinates of cell center points."
  [r c]
  (for [j (range r) i (range c)]
    [(/ (+ i 0.5) c) (/ (+ j 0.5) r)]))
