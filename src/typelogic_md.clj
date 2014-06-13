(ns typelogic-md
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(assert (= (class "hoge") java.lang.String))

(assert (= (class []) clojure.lang.PersistentVector))

(declare ann)

(defna ann-do [ctx exprs type]
  ([_ [expr] _] (ann ctx expr type))
  ([_ [expr . exprs'] _]
    (fresh [type] (ann ctx expr type))
    (ann-do ctx exprs' type)))

(defn ann-if [ctx test consequent alternative type]
  (all
    (fresh [type] (ann ctx test type))
    (ann ctx consequent type)
    (ann ctx alternative type)))

(defna ann-list [ctx exprs types]
  ([_ [] []])
  ([_ [expr . exprs'] [type . types']]
    (ann ctx expr type)
    (ann-list ctx exprs' types')))

(defn ann-app [ctx expr type]
  (fresh [types]
    (ann-list ctx expr types)
    (matcha [types] ([[[::fn type . params] . params]]))))

(defna ann-fn [ctx syms exprs params return]
  ([_ [] _ [] _] (ann-do ctx exprs return))
  ([_ [sym . syms'] _ [param . params'] _]
   (fresh [ctx']
     (conso [sym param] ctx ctx')
     (ann-fn ctx' syms' exprs params' return))))

(defna ann-var [ctx expr type]
  ([[[expr type] . _] _ _])
  ([[_ . ctx'] _ _] (ann-var ctx' expr type)))

(defna ann [ctx expr type]
  ([_ ['do . exprs] _]
    (ann-do ctx exprs type))
  ([_ ['if test consequent alternative] _]
    (ann-if ctx test consequent alternative type))
  ([_ ['fn syms . exprs] [::fn return . params]]
    (ann-fn ctx syms exprs params return))
  ([_ _ _]
    (pred expr seq?)
    (ann-app ctx expr type))
  ([_ _ _] (ann-var ctx expr type))
  ([_ _ _] (is type expr class)))

(defn check
  ([expr] (check [] expr))
  ([ctx expr]
    (run* [type] (ann ctx expr type))))

(check [['a java.lang.String]] 'a)

(check '(do ""))

(check '(if true ["hoge"] []))

(check '(fn [a] a))

(check '((fn [a] a) ""))
