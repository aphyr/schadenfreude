(defproject schadenfreude "0.1.2-SNAPSHOT"
  :description "A benchmarking tool."
  :url "http://github.com/aphyr/schadenfreude"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main schadenfreude.demo
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-tools "1.1.2"]
                 [interval-metrics "0.0.2"]
                 [incanter/incanter-core "1.5.4"]
                 [incanter/incanter-charts "1.5.4"]])
