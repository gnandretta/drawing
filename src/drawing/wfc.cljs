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
  (vec (map #(vec (range (count tiles)))
            (range (* r c)))))

(defn pick [pattern]
  (let [entropies (map count pattern)
        uncollapsed-min-entropy (apply min (remove (partial = 1) entropies))]
    (->> entropies
         (map-indexed vector)
         (filter #(= uncollapsed-min-entropy (second %)))
         (shuffle)
         (first)
         (first))))

(defn collapsed? [pattern i]
  (= (count (get pattern i)) 1))

(defn collapse [pattern i]
  (assoc pattern i [(rand-nth (get pattern i))]))

(defn adjacent-indexes [[r c] i]
  [(let [n (- i c)] (if (>= n 0) n))
   (if (not= (mod (inc i) c) 0) (inc i))
   (let [n (+ i c)] (if (< n (* r c)) n))
   (if (not= (mod i c) 0) (dec i))])

(defn get-adjacency-constraint [pattern i direction]
  (apply set/union (map #(get-in tiles [% direction]) (get pattern i (range (count tiles))))))

(defn constrain [pattern [r c] i]
  (->> (map (fn [j direction]
              (get-adjacency-constraint pattern j direction))
            (adjacent-indexes [r c] i)
            [:down :left :up :right])
       (apply set/intersection (set (get pattern i)))
       (vec)
       (assoc pattern i)))

(defn propagate [pattern [r c]]
  (reduce (fn [pattern i]
            (cond-> pattern
                    (not (collapsed? pattern i)) (constrain [r c] i)))
          pattern
          (range (count pattern))))
