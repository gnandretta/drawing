(ns drawing.math)

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
