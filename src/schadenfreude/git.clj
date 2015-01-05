(ns schadenfreude.git
  "Make charts comparing git commits.
  
  This is horrible. I am so so sorry."
  (:require clojure.java.shell)
  (:use [clojure.string :only [trim-newline]]
        [incanter.core :only [save]]
        [schadenfreude.core :only [record-suite latency-plot throughput-plot]])
  (:import (java.io File)))

(defn sh
  "Like clojure.java.shell.sh but logs commands, stdout, and stderr, plus
  throws on nonzero state."
  [& args]
  (apply println "$" args)
  (let [r (apply clojure.java.shell/sh args)]
    (when-not (empty? (:out r))
      (print (:out r)))
    (when-not (empty? (:err r))
      (print (:err r)))
    (assert (zero? (:exit r)))
    r))

(defn cwd
  "Current working directory"
  []
  (System/getProperty "user.dir"))

(defn head
  "Returns the symbolic reference for HEAD, *OR* an SHA1 if HEAD is detached."
  [dir]
  (try
    (trim-newline (:out 
                    (sh "git" "symbolic-ref" "--short" "HEAD" :dir dir)))
    (catch AssertionError e
      (trim-newline (:out (sh "git" "rev-parse" "HEAD" :dir dir))))))

(defn human-version
  "Returns a human-readable version name for the current commit in directory
  (default CWD)."
  ([] (human-version (cwd))) 
  ([dir]
   (let [status (sh "git" "status" "-s" :dir dir)]
     (cond
       ; Error
       (< 0 (:exit status))
       "not a git repository"

       ; Unchanged commit.
       (empty? (:out status))
       (str
         (trim-newline (:out (sh "git" "show" "-s" "--format=%ci" "HEAD" 
                                 :dir dir)))
         " "
         (trim-newline (:out (sh "git" "rev-parse" "--short" "HEAD"
                                 :out "UTF-8"
                                 :dir dir))))

       ; Changed commit.
       :else
       "HEAD"))))

(defn checkout
  "Checks out a version in directory."
  [dir version]
  (sh "git" "checkout" version :dir dir))

(defn safe-to-stash?
  "Does dir have any unadded files; e.g., would git stash *not* preserve them?"
  [dir]
  (empty? (:out (sh "git" "ls-files" "--other" "--exclude-standard" :dir dir))))

(defn stash
  "Stashes uncommitted changes and returns whether a stash was created."
  [dir]
  (not= "No local changes to save\n"
        (:out (sh "git" "stash" "save" "schadenfreude" :dir dir))))

(defn stash-pop
  "Pops the last stash in dir."
  [dir]
  (sh "git" "stash" "pop" :dir dir))

(defmacro with-stash
  "Stashs uncommitted changes in dir for the duration of body."
  [dir & body]
  `(do
     (when-not (safe-to-stash? ~dir)
       (throw (RuntimeException. 
                (str "There are untracked files in " ~dir 
                     ", aborting to avoid losing your changes."))))
     (let [head# (head ~dir)]
       (if (stash ~dir)
         (try ~@body
           (finally
             (checkout ~dir head#) 
             (stash-pop ~dir)))
         (let [res# (do ~@body)]
           (checkout ~dir head#)
           res#)))))

(defn map-versions
  "Calls (f version) with dir checked out to each version. A special version
  \"HEAD\" refers to the current state of the directory, including uncommitted
  changes. Preserves uncommitted changes by using git stash. Returns a map of
  version -> (f version)."
  [f dir versions]
  (let [heads? (some #{"HEAD"} versions)
        versions (remove #{"HEAD"} versions)
        m (with-stash
            dir
            (reduce (fn [m version]
                      (checkout dir version)
                      (println "On version" version)
                      (assoc m version (f version)))
                    {}
                    versions))]
    (if heads?
      ; Run on uncommitted head.
      (assoc m "HEAD" (f "HEAD"))
      m)))

(defn compare-versions
  "Writes latency and throughput plots comparing a test suite across a series
  of versions."
  [dir versions suite]
  (let [recorded (map-versions 
                   (fn [version]
                     (record-suite suite))
                   dir versions)]
    (dorun
      (for [run (:runs suite)]
        (let [name (:name run)
              runs (mapcat (fn [[version suite]]
                             (keep (fn [run]
                                     (when (= name (:name run))
                                       ; This is one of our runs. Add the
                                       ; version so we can tell them apart.
                                       (assoc run :name
                                              (str name " " version))))
                                   (:runs suite)))
                           recorded)]
          (doseq [l [0 0.5 0.95 0.99 0.999 1]]
            (save (latency-plot l runs) (str name " latency " l ".png") :width 1024))
          (save (throughput-plot runs)
                (str name " throughput.png")
                :width 1024))))))
