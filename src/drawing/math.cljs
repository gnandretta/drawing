(ns drawing.math
  (:require ["random$default" :as random]
            ["simplex-noise" :refer [createNoise2D]]))

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
  [t]
  (noise-2d-generator t 0))
