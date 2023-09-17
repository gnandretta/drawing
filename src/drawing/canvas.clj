(ns drawing.canvas)

(defmacro draw [drawing params & args]
  (let [mt# (if (symbol? drawing) `(meta (var ~drawing)) {})]
    `(draw* ~mt# ~drawing ~params ~@args)))
