(ns schadenfreude.core-test
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
                      (let [before (atom 0)
                            x      (atom 0)
                            after  (atom 0)
                            run (record {:n n
                                        :threads threads 
                                        :before #(swap! before inc)
                                        :after  #(swap! after  inc)
                                        :f      #(swap! x      inc)})]
                        (is (= (col-names (:record run)) [:time :latency]))
                        (is (= n (count (:rows (:record run)))))
                        (is (= n @x))
                        (is (= 1 @before))
                        (is (= 1 @after))))]
           (rec 0 0)
           (rec 1 1)
           (rec 10 1)
           (rec 10 2)
           (rec 43 5)))
