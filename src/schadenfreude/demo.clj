(ns schadenfreude.demo
  (:use [schadenfreude.git :only [compare-versions]]
        [clojure.stacktrace :only [print-cause-trace]]))

(defn suite
  [dir]
  {:before #(prn "setup")
   :after #(prn "teardown" %)
   :runs [{:name "demo"
           :n 10000
           :threads 4
           :before #(prn "before" %)
           :after #(prn "after" %)
           :f (fn [_] (Thread/sleep 10))}]})

(defn -main
  [dir & versions]
  (try
    (compare-versions dir versions (suite dir))
    (flush)
    (System/exit 0)
    (catch Throwable t
      (print-cause-trace t)
      (flush)
      (System/exit 1))))
