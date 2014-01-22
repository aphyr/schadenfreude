(ns schadenfreude.core-test
  (:import java.util.concurrent.atomic.AtomicLong)
  (:require [incanter.core :as incanter])
  (:use clojure.test
        incanter.core
        schadenfreude.core))

(deftest divide-evenly-test
         (are [n m vec] (= (divide-evenly n m) vec)
              0  0  []
              10 0  []
              10 1  [10]
              10 2  [5 5]
              10 3  [3 3 4]
              10 4  [2 2 2 4]
              10 5  [2 2 2 2 2]
              10 6  [1 1 1 1 1 5]
              10 7  [1 1 1 1 1 1 4]
              10 8  [1 1 1 1 1 1 1 3]
              10 9  [1 1 1 1 1 1 1 1 2]
              10 10 [1 1 1 1 1 1 1 1 1 1])

         (is (thrown? java.lang.AssertionError (divide-evenly 1 -1)))
         (is (thrown? java.lang.AssertionError (divide-evenly -1 -1)))
         (is (thrown? java.lang.AssertionError (divide-evenly 1 2))))

(deftest name-dataset-test
         (let [d     (dataset [:x :y] [[1 2] [3 4]])
               named (dataset [:x :y :name] [[1 2 "hi"] [3 4 "hi"]])]
           (is (= (name-dataset d "hi") named))))


(deftest transpose-test
         (are [x y] (and (= (transpose x) y)
                         (= (transpose y) x))
              [[]] [[]]
              [[1]] [[1]]
              [[1 2]] [[1] [2]]
              [[1 2 3] [4 5 6]] [[1 4] [2 5] [3 6]]))

(deftest project-dataset-test
         (let [ds (dataset [:a :b] [[1 2] [3 4]])]
           (is (= (project-dataset ds [:b :c])
                  (dataset [:b :c] [[2 nil] [4 nil]])))))

(deftest merge-datasets-test
         (let [a (dataset [:t :a] [[1 :a1] [2 :a2]])
               b (dataset [:t :b] [[1 :b1] [2 :b2]])]
           (is (= (merge-datasets a b)
                  (dataset [:t :a :b] [[1 :a1 nil]
                                       [2 :a2 nil]
                                       [1 nil :b1]
                                       [2 nil :b2]])))))


(deftest record-test
  (letfn [(rec [n threads]
            (let [before (atom nil)
                  x      (atom 0)
                  after  (atom nil)
                  run    (record
                           {:n n
                            :threads threads
                            :before (fn []  (reset! before (System/nanoTime)))
                            :after  (fn [_] (reset! after (System/nanoTime)))
                            :f      (fn [_] (swap! x     inc))})
                  dt      (* time-scale (- @after @before))]
              (is (= (col-names (:record run))
                     [:time
                      :throughput
                      :latency-0
                      :latency-0.5
                      :latency-0.95
                      :latency-0.99
                      :latency-0.999
                      :latency-1]))
              (is (pos? @before))
              (is (pos? @after))
              (is (= (count (:rows (:record run)))
                     (long (Math/ceil (/ dt sample-interval)))))
              (is (= n @x))))]
    (rec 1 1)
    (rec 10 1)
    (rec 10 2)
    (rec 43 5)
    (rec 10000000 4)))

; Compare j.u.c. atomiclong to swap!
(deftest plot-test
  (let [n               10000000
        threads         4
        juc  (record {:n       n
                      :threads threads
                      :before  #(AtomicLong. 0)
                      :f       (fn [^AtomicLong a] (.incrementAndGet a))})
        swap (record {:n       n
                      :threads threads
                      :before  #(atom 0)
                      :f       #(swap! % inc)})]
    ; Throughput
    (incanter/save (throughput-plot [juc swap])
                   "throughput.png"
                   :width 1024)

    ; Latencies
    (doseq [l [0 0.5 0.95 0.99 0.999 1]]
      (incanter/save (latency-plot l [juc swap])
                     (str "latency-" l ".png")
                     :width 1024))))
