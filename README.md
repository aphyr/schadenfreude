# Schadenfreude

Schadenfreude is a German word, meanining "happiness at the misfortune of
others". As a Clojure library, it helps you take pleasure in the suffering of
machines.

Schadenfreude can record time series measurements, produce latency and
throughput plots over time, and compare multiple versions of a git repository
to understand how your changes affect performance.

## Runs

A Run is a single benchmark, represented as a map:

```clj
{:name    "adding with atoms" ; A name which uniquely describes the benchmark.
 :before  (fn [] (atom 0))    ; A function called before starting the benchmark.
                              ; The return value of :before is passed to each
                              ; invocation of :f.
 :f       (fn [counter] (swap! counter inc)) ; A function we want to measure.
 :after   (fn [counter])      ; A function called to clean up afterwards
 :n       50000               ; How many times to call f.
 :threads 5                   ; How many threads should call f?
 :prime   true}               ; Should we do a dry run first, to warm up?
```

## Suites

A Suite is a set of runs to perform in order:

```clj
{:before (fn [] some-state)            ; A function called once, before 
                                       ; starting runs.
 :runs [{...} {...} ...]               ; A sequence of runs.
 :after (fn [state] (teardown state))} ; A function to clean up afterwards.
```

If your suite has a :before fn, its return value will be passed to each run's
:before. That way, your runs can depend on state initialized by (:before
suite).

## Usage

```clj
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
```

lein run ~/some-git-repo a542f9d3 HEAD

When you run this program with a git repo, and a list of versions (SHA1s, tags, or HEAD for the current state), Schadenfreude will check out each version in turn, run the suite, and record the results. Then it generates latency and throughput plots in the working directory, comparing the performance of each run across different git versions.

Or you can directly record runs (or whole suites) yourself, using
schadenfreude.core. See (record-suite) and (record) to collect data, and (throughput-plot) and (latency-plot) to compare recorded runs.

## Caveats

Schadenfreude's core is a horrible, buggy, stateful mess, with complicated
functions and a poorly defined API. If you have sweeping ideas about how to
make it better, please do. :D

## License

Copyright © 2013 Kyle Kingsbury <aphyr@aphyr.com>

Distributed under the Eclipse Public License, the same as Clojure.
