(ns typelogic-md
  (:refer-clojure :exclude [==])
  (:require [clojure.walk :refer [macroexpand-all]]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.nominal :as nom]))

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

(defn ann-tag [sym type]
  (fresh [tag]
    (is tag sym (comp :tag meta))
    (pred tag (complement nil?))
    (is type tag resolve)))

(defna ann-fn [ctx syms exprs params return]
  ([_ [] _ [] _] (ann-do ctx exprs return))
  ([_ [sym . syms'] _ [param . params'] _]
   (fresh [ctx']
     (conda [(ann-tag sym param)]
            [succeed])
     (conso [sym param] ctx ctx')
     (ann-fn ctx' syms' exprs params' return))))

(defne ann-fns [ctx exprs type]
  ([_ [[syms . body] . _] [::fn return . params]]
     (ann-fn ctx syms body params return))
  ([_ [_ . exprs'] _]
     (ann-fns ctx exprs' type)))

(defna ann-var [ctx sym type]
  ([[[sym type] . _] _ _])
  ([[_ . ctx'] _ _] (ann-var ctx' sym type)))

(defna ann [ctx expr type]
  ([_ ['do . exprs] _]
    (ann-do ctx exprs type))
  ([_ ['if test consequent alternative] _]
    (ann-if ctx test consequent alternative type))
  ([_ ['fn* name [syms . exprs]] [::fn return . params]]
    (pred name symbol?)
    (fresh [ctx']
      (conso [name type] ctx ctx')
      (ann-fn ctx' syms exprs params return)))
  ([_ ['fn* [syms . exprs]] [::fn return . params]]
    (ann-fn ctx syms exprs params return))
  ([_ _ _]
    (pred expr seq?)
    (ann-app ctx expr type))
  ([_ _ _] (ann-var ctx expr type))
  ([_ _ _] (is type expr class)))

(defn check
  ([expr] (check [] expr))
  ([ctx expr]
    (run* [type] (ann ctx (macroexpand-all expr) type))))

(check [['a java.lang.String]] 'a)

(check '(do ""))

(check '(if true ["hoge"] []))

(check '(fn [a] a))

(check '((fn [a] a) ""))

(check '(fn [^String s] s))

(check '((fn [^String s] s) 0))

(check '(fn f [a] (if true a (f ""))))
