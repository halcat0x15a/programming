(ns complex
  (:refer-clojure :exculde [+ - * /])
  (:require [clojure.algo.generic :refer (root-type)]
            [clojure.algo.generic.arithmetic :refer (+ - * / zero one)]))

(defrecord Complex [real imaginary])

(defmethod + [Complex Number] [c n] (+ c (Complex. n 0)))
(defmethod + [Number Complex] [n c] (+ (Complex. n 0) c))
(defmethod + [Complex Complex] [c c']
  (Complex. (+ (:real c) (:real c')) (+ (:imaginary c) (:imaginary c'))))

(assert (= (+ (Complex. 1 1) (Complex. 1 1)) (Complex. 2 2)))
(assert (= (+ (Complex. 1 1) 1) (Complex. 2 1)))
(assert (= (+ (Complex. 1 1) zero) (Complex. 1 1)))

(defmethod * [Complex Number] [c n] (* c (Complex. n 0)))
(defmethod * [Number Complex] [n c] (* (Complex. n 0) c))
(defmethod * [Complex Complex] [c c']
  (Complex. (- (* (:real c) (:real c')) (* (:imaginary c) (:imaginary c')))
            (+ (* (:real c) (:imaginary c')) (* (:imaginary c) (:real c')))))

(assert (= (* (Complex. 1 1) (Complex. 1 1)) (Complex. 0 2)))
(assert (= (* (Complex. 1 1) 0) (Complex. 0 0)))
(assert (= (* (Complex. 1 1) one) (Complex. 1 1)))
