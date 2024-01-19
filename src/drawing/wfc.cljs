(ns drawing.wfc
  (:require [clojure.set :as set]
            [drawing.canvas :as c]))

; the coding train, coding challenge 171: wave function collapse (https://www.youtube.com/watch?v=rI_y2GAlQFM)

(defn make-draw-tile-fn [rects]
  (fn [ctx] (-> ctx
                (c/set-fill-style "aquamarine")
                (c/fill-rect [40 40])
                (c/set-fill-style "coral")
                (c/j> (doseq [[xy d] rects] (c/fill-rect ctx xy d))))))

(def tiles [{:draw-fn (make-draw-tile-fn [])
             :up      #{0 1}
             :right   #{0 2}
             :down    #{0 3}
             :left    #{0 4}}
            {:draw-fn (make-draw-tile-fn [[[15 0] [10 15]] [[0 15] [40 10]]])
             :up      #{2 3 4}
             :right   #{1 3 4}
             :down    #{0 3}
             :left    #{1 2 3}}
            {:draw-fn (make-draw-tile-fn [[[25 15] [15 10]] [[15 0] [10 40]]])
             :up      #{2 3 4}
             :right   #{1 3 4}
             :down    #{1 2 4}
             :left    #{0 4}}
            {:draw-fn (make-draw-tile-fn [[[15 25] [10 15]] [[0 15] [40 10]]])
             :up      #{0 1}
             :right   #{1 3 4}
             :down    #{1 2 4}
             :left    #{1 2 3}}
            {:draw-fn (make-draw-tile-fn [[[0 15] [15 10]] [[15 0] [10 40]]])
             :up      #{2 3 4}
             :right   #{0 2}
             :down    #{1 2 4}
             :left    #{1 2 3}}])

(defn tile-reference []
  (let [spacing 70
        d [150 (* (count tiles) spacing)]
        ctx (c/append ::tile-reference d)]
    (doseq [i (range (count tiles)) :let [draw-tile (get-in tiles [i :draw-fn])]]
      (-> ctx
          (c/save)
          (c/translate [0 (* i spacing)])
          (c/set-fill-style "black")
          (c/fill-text i [0 10])
          (c/translate [15 0])
          (draw-tile)
          (c/restore)))))

(defn make-pattern [[r c]]
  {:v (vec (map #(vec (range (count tiles)))
             (range (* r c))))})

(defn pick [v]
  (let [entropies (map count v)
        uncollapsed-min-entropy (apply min (remove (partial = 1) entropies))]
    (->> entropies
         (map-indexed vector)
         (filter #(= uncollapsed-min-entropy (second %)))
         (shuffle)
         (first)
         (first))))

(defn collapsed? [v i]
  (= (count (get v i)) 1))

(defn collapse [v i]
  (assoc v i [(rand-nth (get v i))]))

(defn constrain [v [r c] i]
  (let [all-tiles (range (count tiles))
        [t r l b] [(let [n (- i c)] (if (>= n 0) n))
                   (let [n (inc i)] (if (not= (mod n c) 0) n))
                   (let [n (+ i c)] (if (< n (* r c)) n))
                   (if (not= (mod i c) 0) (dec i))]]
    (->> (map (fn [j direction]
                (apply set/union (map (fn [ti] (get-in tiles [ti direction]))
                                      (get v j all-tiles))))
              [t r l b]
              [:down :left :up :right])
         (apply set/intersection (set (get v i)))
         (vec)
         (assoc v i))))

(defn propagate [v [r c]]
  (reduce (fn [v i] (cond-> v (not (collapsed? v i)) (constrain [r c] i)))
          v
          (range (count v))))

(defn draw-pattern [ctx v [r c] [w h]]
  (doall (for [i (range (count v))
               :let [element (get v i)
                     draw-fn (get-in tiles [(first element) :draw-fn])]
               :when (= (count element) 1)]
           (-> ctx
               (c/save)
               (c/translate [(* (mod i c) w) (* (quot i r) h)])
               (draw-fn)
               (c/restore)))))

(defn drawing []
  (let [d [600 1000]
        ctx (c/append ::drawing d)
        [r c] [5 5]
        pattern (loop [pattern (make-pattern [r c])]
                  (let [i (pick (:v pattern))]
                    (if (and i (not (collapsed? (:v pattern) i)))
                      (recur (-> pattern (update :v collapse i)
                                 (update :v propagate [r c])))
                      pattern)))]
    (draw-pattern ctx (:v pattern) [r c] [40 40])))
