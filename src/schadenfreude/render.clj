(ns schadenfreude.render
  "Renders datasets."
  (:use (incanter core charts)))

(defn write-chart [chart opts]
  (let [file (str "bench/

