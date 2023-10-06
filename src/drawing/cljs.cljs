(ns drawing.cljs)

(defn jump->
  "Returns its first argument. Useful for side effects in the middle of the body
   of a thread-first macro (->). "
  [x & _]
  x)
