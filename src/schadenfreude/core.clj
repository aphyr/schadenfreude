(ns schadenfreude.core
  "A *run* is the fundamental unit of benchmarking in Schadenfreude. Each run
  specifies a single function which we want to understand the behavior of, and
  information about how to execute that function. Runs are just maps:

   {:before  ; A function to call before beginning.
    :f       ; A function to actually benchmark.
    :after   ; A function to call at shutdown.
    :n       ; How many times to evaluate f
    :threads ; Number of threads}"
  (:require [interval-metrics.core :as metrics]
            [interval-metrics.measure :as measure])
  (:use schadenfreude.util
        incanter.core
        incanter.charts
        [clojure.tools.logging :only [info warn]]
        [clojure.stacktrace :only [print-cause-trace]]))

(def time-scale
  "Recorded time (nanos) * time-scale = time, in seconds."
  1/1000000000)

(def sample-interval
  "How often to sample collected metrics, in seconds."
  1/10)

(defn divide-evenly
  "Divides an integer n into a vector of m roughly equal integers."
  [n m]
  (assert (<= 0 m n))
  (if (= 0 m)
    []
    (concat
      (replicate (dec m) (quot n m))
      [(- n    (* (dec m) (quot n m)))])))

(defn progress-fut
  "In a future, periodically displays a progressbar given a reference to a set
  of counters and a total."
  [counters total]
  (assert (pos? total))
  (future
    (loop []
      (let [i (reduce (fn [sum c] (+ sum @c)) 0 counters)]
        (render-progress
          {:width 80
           :i i
           :total total})
        (when (< i total)
          (Thread/sleep 100)
          (recur))))
    (print "\n")))

(defn measure-thread
  "In a new thread, measures the given metric every dt seconds. Returns a delay
  which, when dereferenced, stops the measurement thread and returns a sequence
  of observations."
  [dt metric]
  (let [observations (atom [])
        measurer     (measure/periodically dt
                       (swap! observations conj (metrics/snapshot! metric)))]
    (delay
      (measurer)
      (swap! observations conj (metrics/snapshot! metric))
      @observations)))

(defn record-thread
  "Evaluates f repeatedly for a run, single-threaded, updating the given metric
  with every latency measurement."
  [run before-val counter metric]
  (let [n         (get run :n 1)
        progress  (max 1 (int (/ n 100)))
        f         (:f run)]
    (try
      (dotimes [i n]
        (measure/measure-latency metric
          (f before-val))

        ; Update counter
        (when (zero? (mod i progress))
          (reset! counter (inc i))))

      (catch Throwable t
        (warn t)
        (throw t))
      (finally
        (reset! counter n)))))

(defn doublen
  "Converts x to a double when x is non-nil."
  [x]
  (when x (double x)))

(defn record
  "Record executes a run and returns a [time, latency] dataset."
  [run]
  (when (:prime run)
    (record (dissoc run :prime)))

  ; Before callback
  (let [before-val   (when-let [b (:before run)] (b))
        thread-count (get run :threads 1)
        n            (get run :n 1)
        counters     (vec (take thread-count (repeatedly #(atom 0))))
        progress     (progress-fut counters n)
        metric       (metrics/rate+latency
                       {:rate-scale    :seconds
                        :latency-scale :milliseconds
                        :quantiles     [0 0.5 0.95 0.99 0.999 1]})

        ; Start measurement thread
        measurer (measure-thread sample-interval metric)

        ; Start threads to run f
        workers (map-indexed
                  (fn [thread-id n]
                    (future (record-thread
                                (assoc run :n n)
                                before-val
                                (counters thread-id)
                                metric)))
                  (divide-evenly n thread-count))]

    ; Run threads
    (dorun workers)

    ; Wait for threads
    (dorun (map deref workers))

    ; Get observations and kill measurement thread
    (let [observations @measurer]
      ; Finish up
      (when-let [a (:after run)] (a before-val))

      ; Wait for progress thread
      @progress

      ; Convert to dataset. Joins all tapes together, converts units to
      ; seconds, normalizes times relative to start time.
      (let [t0        (apply min (map :time observations))]
        (assoc run :record
               (dataset [:time
                         :throughput
                         :latency-0
                         :latency-0.5
                         :latency-0.95
                         :latency-0.99
                         :latency-0.999
                         :latency-1]
                        (map (fn [o]
                               (let [l (:latencies o)]
                                 [(double (- (:time o) t0))
                                  (double (:rate o))
                                  (doublen (get l 0))
                                  (doublen (get l 0.5))
                                  (doublen (get l 0.95))
                                  (doublen (get l 0.99))
                                  (doublen (get l 0.999))
                                  (doublen (get l 1))]))
                             observations)))))))

(defn record-suite
  "Records a suite of runs."
  [suite]
  (let [; First, set up the suite with (:before).
        before-val (when-let [b (:before suite)]
                     (b))
        ; Rewrite runs to call :before with before-val.
        rewritten-runs (map (fn [run]
                              (if-let [b (:before run)]
                                (assoc run :before #(b before-val))
                                run))
                         (:runs suite))
        ; Record each run
        recorded (doall (map record rewritten-runs))]
    ; Tear down
    ((:after suite) before-val)
    ; Return completed suite
    (assoc suite :runs recorded)))

(defn transpose
  "Lazy transposition of a seq of seqs"
  [sequences]
  (if (some empty? sequences)
    '(())
    (apply map (fn [& args] args) sequences)))

(defn name-dataset
  "Adds a :name column, and names every row of a dataset."
  [dataset name]
  (-> dataset
    (conj-cols (repeat (count (:rows dataset)) name))
    (col-names (concat (col-names dataset) [:name]))))

(defn project-dataset
  "Given a dataset and a column basis, projects the dataset's columns to that
  basis. Missing columns are filled with nil."
  [ds basis]
  (let [blanks (repeat (count (:rows ds)) nil)]
    (dataset basis
             (transpose
               (map (fn [col]
                      (or ($ col ds) blanks))
                    basis)))))

(defn merge-datasets
  "Merges several datasets together."
  [& datasets]
  (let [basis (distinct (mapcat col-names datasets))
        projected (map #(project-dataset % basis) datasets)
        merged (apply conj-rows projected)]
    (col-names merged basis)))

(defn log-plot
  "Changes a plot to be logarithmic."
  [plot]
  (let [p (.getPlot plot)
        label (.. p (getRangeAxis) (getLabel))]
  (.setRangeAxis p
    (org.jfree.chart.axis.LogarithmicAxis. label)))
  plot)

(defn latency-plot
  "Takes a list of recorded runs and generates a timeseries chart comparing
  their latencies. Latency is one of [0, 0.5, 0.95, 0.99, 0.999, 1]. Defaults
  to median (0.5) latency."
  ([runs] (latency-plot 0.5 runs))
  ([latency runs]
   (let [latency (keyword (str "latency-" latency))]
;     (log-plot
       (reduce
         (fn [plot run]
           (add-lines plot
                      :time
                      latency
                      :data         (:record run)
                      :series-label (:name run)))
         (xy-plot :time
                  latency
                  :data (:record (first runs))
                  :title (str latency " latency")
                  :x-label "Time (s)"
                  :y-label "Latency (ms)"
                  :legend (< 1 (count runs))
                  :series-label (:name (first runs)))
         (rest runs)))))

(defn throughput-plot
  "Takes a list of recorded runs and generates a timeseries chart comparing
  their throughputs."
  [runs]
  (assert (not (empty? runs)))
  (reduce
    (fn [plot run]
      (add-lines plot
                 :time
                 :throughput
                 :data         (:record run)
                 :series-label (:name run)))
    (xy-plot :time
             :throughput
             :data          (:record (first runs))
             :title         "Throughput"
             :x-label       "Time (s)"
             :y-label       "Throughput (hz)"
             :legend        (< 1 (count runs))
             :series-label  (:name (first runs)))
    (rest runs)))
