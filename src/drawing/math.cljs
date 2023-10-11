(ns drawing.math
  (:require ["random$default" :as random]
            ["simplex-noise" :refer [createNoise2D]]))

(defn- v-op [op v & xs]
  (reduce (fn [acc x] (mapv op acc (cond (fn? x) (repeatedly x)
                                         (seqable? x) x
                                         :else (repeat x))))
          v
          xs))

(defn v+
  "Adds to each element of vector v numbers, corresponding element of vectors
   (must be same size as v, at least), or the result of evaluating fns."
  [v & xs]
  (apply v-op + v xs))

(defn v-
  "Subtracts from each element of vector v numbers, corresponding element of
   vectors (must be same size as v, at least), or the result of evaluating fns."
  [v & xs]
  (apply v-op - v xs))

(defn v*
  "Multiplies each element of vector v by numbers, corresponding element of
   vectors (must be same size as v, at least), or the result of evaluating fns."
  [v & xs]
  (apply v-op * v xs))

(defn v-div
  "Divides each element of vector v by numbers, corresponding element of vectors
   (must be same size as v, at least), or the result of evaluating fns."
  [v & xs]
  (apply v-op / v xs))

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

(defn adj                                                   ; TODO consider switching args order, see polar notation
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

(defn pi
  "Multiplies PI by the given numbers."
  [& ns]
  (apply * js/Math.PI ns))

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
  "Returns a random integer  between a (inclusive) and b (exclusive)."
  [a b]
  (+ a (rand-int (- b a))))

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

(defn grid
  "Returns a lazy sequence of normalized coordinates of the intersections in a
   grid with v (or n) vertical lines and h (or n) horizontal lines."
  ([n] (grid n n))
  ([v h] (for [j (range h) i (range v)]
           [(/ i (dec v)) (/ j (dec h))])))
