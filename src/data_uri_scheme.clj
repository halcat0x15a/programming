(ns data-uri-scheme
  (:require [clojure.data.codec.base64 :refer (encode)])
  (:import [java.nio.file Paths Files]))

(defn -main [path]
  (->> (Paths/get path (make-array String 0))
       Files/readAllBytes
       encode
       String.
       (str "data:image/png;base64,")
       println))
