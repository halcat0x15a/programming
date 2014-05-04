---
layout: default
title: algo.generic
---

# algo.generic

Clojureの関数を総称的な関数として提供するライブラリです.

## Install

以下は`project.clj`の記述例です.

```clojure
(defproject programming "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/algo.generic "0.1.1"]])
```

## Generic functions

簡単な例を見てみましょう.

```clojure
(refer-clojure :exculde [+ - * /])
(require '[clojure.algo.generic.arithmetic :refer (+ - * /)])

(assert (= (+ 1 2) 3))
(assert (= (- 1 2) -1))
(assert (= (* 1 2) 2))
(assert (= (/ 1 2) 1/2))
```

単純な四則演算です.

java.lang.Numberに対してはclojure.coreの関数と変わりない動作をします.

algo.generic.arithmeticの`zero-type`と`one-type`について見てみましょう.

```clojure
(refer-clojure :exculde [+ - * /])
(require '[clojure.algo.generic.arithmetic :refer (+ - * / zero one)])

(assert (= (+ 2 zero) 2))
(assert (= (- 3 zero) 3))
(assert (= (* 5 one) 5))
(assert (= (/ 7 one) 7))
(assert (= (+) zero))
(assert (= (*) one))
```

`zero`は加算における単位元,`one`は乗算における単位元として働いています.

次に,複素数とそれに対する演算を定義してみましょう.

```clojure
(ns complex
  (:refer-clojure :exculde [+ - * /])
  (:require [clojure.algo.generic.arithmetic :refer (+ - * / zero one)]))

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
```

`+`と`*`が複素数に対して使えるようになりました.

このように,algo.genericの関数はMultimethodで定義されているため,外部からの拡張が可能です.

さらに,algo.genericの関数を使った定義ならば,その関数もまた拡張が可能です.

```clojure
(refer-clojure :exculde [*])
(require '[clojure.algo.generic.arithmetic :refer (*)])
(require '[complex :refer (->Complex)])

(defn square [x] (* x x))

(assert (= (square 3) 9))
(assert (= (square (->Complex 1 -1)) (->Complex 0 -2)))
```

また,四則演算だけでなく,モジュール毎に総称的な関数が定義されています.

* algo.generic.arithmetic
* algo.generic.collection
* algo.generic.comparison
* algo.generic.math-functions
* algo.generic.functor
