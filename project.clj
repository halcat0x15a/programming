(defproject programming "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]
                 [org.clojure/algo.generic "0.1.1"]
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/data.codec "0.1.0"]]
  :plugins [[lein-cljsbuild "0.3.4"]]
  :cljsbuild {:builds [{}]
              :crossovers [anyware.core]})
