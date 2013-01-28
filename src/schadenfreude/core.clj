(ns schadenfreude.core
  "A *run* is the fundamental unit of benchmarking in Schadenfreude. Each run
  specifies a single function which we want to understand the behavior of, and
  information about how to execute that function. Runs are just maps:

   {:before  ; A function to call before beginning.
    :f       ; A function to actually benchmark.
    :after   ; A function to call at shutdown.
    :n       ; How many times to evaluate f
    :threads ; Number of threads}"
  (:use schadenfreude.util
        incanter.core
        incanter.charts
        [clojure.tools.logging :only [info warn]]
        [clojure.stacktrace :only [print-cause-trace]]))

(def time-scale
  "Recorded time / time-scale = time, in seconds."
  1/1000000000)

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
  (future
    (loop []
      (let [i (reduce (fn [sum c] (+ sum @c)) 0 counters)]
        (render-progress
          {:width 80
           :i i
           :total total})
        (when (< i total)
          (Thread/sleep 1000)
          (recur))))
    (print "\n")))

(defn record-thread
  "Returns a pair of arrays of times and latencies for a run, single-threaded."
  [run before-val counter]
  (let [n         (get run :n 1)
        progress  (min 1 (int (/ n 100)))
        f         (:f run)
        times     (long-array n)
        latencies (long-array n)]
    (try
      (dotimes [i n]
        (aset-long times i (System/nanoTime))
        (f before-val)
        (aset-long latencies i (- (System/nanoTime) (aget times i)))

        ; Update counter
        (when (zero? (mod i progress))
          (reset! counter (inc i))))

      ; Return tape
      [times latencies]
      (catch Throwable t
        (warn t)
        (throw t))
      (finally
        (reset! counter n)))))

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
        tapes        (atom [])
        ; Start threads to run f
        workers (map-indexed
                  (fn [thread-id n]
                    (Thread. #(let [tape (record-thread 
                                           (assoc run :n n)
                                           before-val
                                           (counters thread-id))]
                                (swap! tapes conj tape))
                             "schadenfreude-record"))
                  (divide-evenly n thread-count))]
    ; Run threads
    (doseq [t workers] (.start t))

    ; Wait for threads
    (while (some #(.isAlive %) workers)
      (Thread/sleep 10))

    ; Wait for progress thread
    @progress

    ; Finish up
    (when-let [a (:after run)] (a before-val))
   
    ; Did any threads crash before completing?
    (when (not= thread-count (count @tapes))
      (throw (RuntimeException. "Some worker threads aborted abnormally!")))

    (let [times     (mapcat first @tapes)
          latencies (mapcat second @tapes)
          t0        (apply min times)]
      ; Convert to dataset. Joins all tapes together, converts units to seconds,
      ; normalizes times relative to start time.
      (assoc run :record
             (dataset [:time :latency]
                      (map (fn [time latency]
                             [(double (* time-scale (- time t0)))
                              (double (* time-scale latency))])
                           times latencies)))))) 

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

(defn throughput
  "Computes a throughput dataset from a recorded run."
  ([run] (throughput run {}))
  ([run opts]
   (assert run)
   (let [ds        ($order :time :asc (:record run))
         times     ($ :time ds)
         _         (assert (< 1 (count times)))
         t1        (first times)
         t2        (last times)
         bin-count (min (dec (count times)) 
                          (max 1 (get opts :bins 100)))
         bin-dt    (/ (- t2 t1) bin-count)
         bin-times (range t1 t2 bin-dt)
         bins      (partition-by #(quot % bin-dt) times)
         points    (drop-last
                     (map (fn [t bin] 
                            [t (/ (count bin) bin-dt)])
                          bin-times bins))]
     (dataset [:time :throughput] points))))

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
  their latencies."
  [runs]
  (log-plot
    (reduce
      (fn [plot run]
        (add-points plot
                    :time :latency
                    :data         (:record run)
                    :series-label (:name run)))

      (scatter-plot :time :latency
                    :data (:record (first runs))
                    :title "Latency"
                    :x-label "Time (s)"
                    :y-label "Latency (s)"
                    :legend (< 1 (count runs))
                    :series-label (:name (first runs)))
      (rest runs))))

(defn throughput-plot
  "Takes a list of recorded runs and generates a timeseries chart comparing
  their throughputs."
  [runs]
  (assert (not (empty? runs)))
  (reduce
    (fn [plot run]
      (add-lines plot
                  :time :throughput
                  :data         (throughput run)
                  :series-label (:name run)))

    (xy-plot :time :throughput
                  :data (throughput (first runs))
                  :title "Throughput"
                  :x-label "Time (s)"
                  :y-label "Throughput (hz)"
                  :legend (< 1 (count runs))
                  :series-label (:name (first runs)))
    (rest runs)))
