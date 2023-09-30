(ns drawing.math
  (:require ["random$default" :as random]
            ["simplex-noise" :refer [createNoise2D]]))

(defn lerp
  "Maps n from [a,b] (defaults to [0,1]) to [c,d] with a linear interpolation."
  ([n c d] (lerp n 0 1 c d))                                ; also works when d < c
  ([n a b c d] (+ (* (- d c)
                     (/ (- n a) (- b a)))
                  c)))

(defn dist
  "Returns the Euclidean distance between (x0,y0) and (x1, y1)."
  [[x0 y0] [x1 y1]]
  (let [dx (- x1 x0) dy (- y1 y0)]
    (js/Math.sqrt (+ (* dx dx) (* dy dy)))))

(defn adj
  "Given the acute angle of a right triangle and the length of its hypotenuse,
   calculates the length of its adjacent side."
  [a h]
  (* (js/Math.cos a) h))

(defn opp
  "Given the acute angle of a right triangle and the length of its hypotenuse,
   calculates the length of its opposite side."
  [a h]
  (* (js/Math.sin a) h))

(defn sides
  "Given the acute angle of a right triangle and the length of its hypotenuse,
   returns a vector with the length of its adjacent and opposite sides."
  [a h]
  [(adj a h) (opp a h)])

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

(defn rand-off
  "Returns a random floating point number between a (inclusive) and b
   (exclusive)."
  [a b]
  (+ a (rand (- b a))))

(defonce rand-std-norm-generator (.normal random))          ; TODO allow to set (and get?) a seed

(defn rand-std-norm
  "Returns a random floating point number sampled from a standard (μ=0, σ=1)
   normal distribution. Use (+ (* σ (rand-std-norm) μ) to obtain a sample from
   another normal distribution."
  []
  (rand-std-norm-generator))

(defonce noise-2d-generator (createNoise2D))                ; TODO see alea npm module

(defn noise
  "Returns a simplex noise value (in the range [-1,1]) at the specified
   coordinates."
  [[x y]]
  (noise-2d-generator x (if (nil? y) 0 y)))

(defn grid
  "Returns a lazy sequence of normalized coordinates of the intersections in a
   grid with v (or n) vertical lines and h (or n) horizontal lines."
  ([n] (grid n n))
  ([v h] (for [j (range h) i (range v)]
           [(/ i (dec v)) (/ j (dec h))])))
