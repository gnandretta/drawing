(ns drawing.math)

(defn weighed-rand-key
  "Given a map where the values are weights, returns a random key based on the
   weights."
  [m]
  (let [interval-ends (reductions + (vals m))
        r (rand (last interval-ends))]
    (loop [[[e k] & eks] (map vector interval-ends (keys m))]
      (when e
        (if (< r e)
          k
          (recur eks))))))
