(ns drawing.ink
  (:require [drawing.canvas :as c]
            [drawing.math :as m]))

; https://thecodingtrain.com/challenges/183-mathematical-marbling

(defn make [cxy r color]
  {:cxy   cxy
   :r     r
   :xys   (map (fn [a] (m/add cxy (m/xy r a))) (range 0 (m/pi 2) (m/pif 100)))
   :color color})

(defn add [inks {:keys [cxy r] :as ink}]
  (let [move-xy (fn [xy] (let [d (m/sub xy cxy)]
                           (m/add cxy (m/mul d (m/sqrt (+ 1 (/ (* r r)
                                                               (m/mag-square d))))))))]
    (conj (mapv #(update % :xys (partial map move-xy)) inks) ink)))

(defn draw [ctx {:keys [xys color]}]
  (-> ctx
      (c/save)
      (c/begin-path)
      (c/move-to (first xys))                               ; TODO c/line?
      (c/j> (doseq [xy (rest xys)] (c/line-to ctx xy)))
      (c/close-path)
      (c/set-fill-style color)
      (c/fill)
      (c/restore)))