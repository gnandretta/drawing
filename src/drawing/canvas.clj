(ns drawing.canvas)

(defmacro draw [drawing & args]
  (let [mt# (if (symbol? drawing) `(meta (var ~drawing)) {})]
    `(draw* ~mt# ~drawing ~@args)))
