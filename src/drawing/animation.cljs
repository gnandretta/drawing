(ns drawing.animation
  (:require [cljs.core.async :refer [chan timeout] :refer-macros [alt! go]]))

(defn- rate-limit [ms]                                      ; this is not very precise, even the following loop won't
  (let [c (chan)]                                           ; run 10 times in a second
    (go (while true                                         ; (go (while true (println (js/Date.)) (<! (timeout 100))))
          (<! (timeout ms))
          (>! c :ready)))
    c))

(defn play
  ([] (play 60))
  ([fps] (let [c (chan)
               ctrl (chan)
               rl (rate-limit (/ 1000 fps))]
           (go (loop [frame 0]
                 (alt! ctrl ([x] (<! ctrl))
                       rl ([] (>! c frame)))
                 (recur (inc frame))))
           [c ctrl])))
