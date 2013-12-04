(ns clojuredocs-search
  (:require [clojure.pprint :refer (pprint)]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.core.async :refer (chan timeout go >! <! alt! >!!)]))

(def deadline 3000)

(defn search [query]
  (-> (str "http://api.clojuredocs.org/search/" query)
      io/as-url
      io/reader
      json/read))

(defn prompt []
  (print "search=> ")
  (flush))

(defn -main []
  (let [query (chan)]
    (go (while true
          (let [query (<! query)
                result (chan)]
            (go (>! result (or (search query) [])))
            (alt! result ([v] (pprint v))
                  (timeout deadline) ([] (println "timeout")))
            (prompt))))
    (while true
      (prompt)
      (>!! query (read-line)))))
