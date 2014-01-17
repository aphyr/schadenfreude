(ns schadenfreude.util)

(defn render-progress
  "Print a progress bar."
  [bar]
  (let [width (:width bar)
        i     (:i bar)
        total (:total bar)
        num-str (str i "/" total)
        dots-width (- width 3 (count num-str))
        dots-count (int (* dots-width (/ i total)))
        space-count (- dots-width dots-count)]
    (locking *out*
      (dotimes [_ width]
        (print "\b"))
      (print "[")
      (print num-str)
      (print " ")
      (dotimes [_ dots-count]
        (print "█"))
      (dotimes [_ space-count]
        (print " "))
      (print "]")
      (flush)
      bar)))

(defn progress
  "Prints a progress bar to the console. Returns a progress bar you can use to
  update."
  [width]
  {:width width})

(defn update-progress
  "Prints an updated progress bar to the console. Returns the updated progress
  bar."
  [bar i total]
  (render-progress (merge bar {:i i :total total})))
