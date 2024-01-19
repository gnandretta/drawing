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

(def sample-tiles [{:draw-fn (make-draw-tile-fn [])
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
        d [150 (* (count sample-tiles) spacing)]
        ctx (c/append ::tile-reference d)]
    (doseq [i (range (count sample-tiles)) :let [draw-tile (get-in sample-tiles [i :draw-fn])]]
      (-> ctx
          (c/save)
          (c/translate [0 (* i spacing)])
          (c/set-fill-style "black")
          (c/fill-text i [0 10])
          (c/translate [15 0])
          (draw-tile)
          (c/restore)))))

(defn make-pattern [in [r c]]
  {:out (vec (map #(vec (range (count in)))
                  (range (* r c))))
   :d   [r c]
   :in  in})

(defn pick [out]
  (let [entropies (map count out)
        uncollapsed-min-entropy (apply min (remove (partial = 1) entropies))]
    (->> entropies
         (map-indexed vector)
         (filter #(= uncollapsed-min-entropy (second %)))
         (shuffle)
         (first)
         (first))))

(defn collapsed? [{:keys [out]} i]
  (= (count (get out i)) 1))

(defn collapse [pattern i]
  (update-in pattern [:out i] (fn [x] [(rand-nth x)])))

(defn constrain [{:keys [out d in] :as pattern} i]
  (let [[r c] d
        [t r l b] [(let [n (- i c)] (if (>= n 0) n))
                   (let [n (inc i)] (if (not= (mod n c) 0) n))
                   (let [n (+ i c)] (if (< n (* r c)) n))
                   (if (not= (mod i c) 0) (dec i))]
        all-tiles (range (count in))]
    (assoc-in pattern [:out i] (->> (map (fn [j direction]
                                           (apply set/union (map #(get-in in [% direction])
                                                                 (get out j all-tiles))))
                                         [t r l b]
                                         [:down :left :up :right])
                                    (apply set/intersection (set (get out i)))
                                    (vec)))))

(defn propagate [{:keys [out] :as pattern}]
  (reduce (fn [pattern i] (cond-> pattern (not (collapsed? out i)) (constrain i)))
          pattern
          (range (count out))))

(defn draw-pattern [ctx {:keys [out d in]} [w h]]
  (let [[r c] d]
    (doall (for [i (range (count out))
                 :let [element (get out i)
                       draw-fn (get-in in [(first element) :draw-fn])]
                 :when (= (count element) 1)]
             (-> ctx
                 (c/save)
                 (c/translate [(* (mod i c) w) (* (quot i r) h)])
                 (draw-fn)
                 (c/restore))))))

(defn drawing []
  (let [d [600 1000]
        ctx (c/append ::drawing d)
        [r c] [5 5]
        pattern (loop [pattern (make-pattern sample-tiles [r c])]
                  (let [i (pick (:out pattern))]
                    (if (and i (not (collapsed? pattern i)))
                      (recur (-> (collapse pattern i) propagate))
                      pattern)))]
    (draw-pattern ctx pattern [40 40])))
