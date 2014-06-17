(ns typelogic-md
  (:refer-clojure :exclude [== methods])
  (:require [clojure.walk :refer [macroexpand-all]]
            [clojure.core.logic :refer :all])
  (:import [java.lang.reflect Method]))

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

(defna subtypes [xs ys]
  ([[] []])
  ([[x . xs'] [y . ys']]
     (project [y]
       (predc x #(isa? % y) (fn [_ _ r s] `(isa? ~(-reify s x r) ~y))))
     (subtypes xs' ys')))

(defn ann-app [ctx expr type]
  (fresh [types]
    (ann-list ctx expr types)
    (matcha [types]
      ([[[::fn type . params] . args]]
         (subtypes args params)))))

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

(defn methods [^Class class method]
  (->> (.getMethods class)
       (filter #(= (.getName ^Method %) (name method)))
       (map (fn [ ^Method m] (cons (.getReturnType m) (.getParameterTypes m))))))

(defne ann-call [ctx methods args type]
  ([_ [[type . params] . _] _ _]
     (fresh [args']
       (ann-list ctx args args')
       (subtypes args' params)))
  ([_ [_ . methods'] _ _]
     (ann-call ctx methods' args type)))

(defn ann-classmethod [ctx name method args type]
  (fresh [class]
    (pred name symbol?)
    (is class name resolve)
    (pred class class?)
    (project [class method]
      (ann-call ctx (methods class method) args type))))

(defn ann-instancemethod [ctx expr method args type]
  (fresh [class]
    (ann ctx expr class)
    (project [class method]
      (ann-call ctx (methods class method) args type))))

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
  ([_ [dot name method . args] _]
     (pred dot #(= % '.))
     (ann-classmethod ctx name method args type))
  ([_ [dot name method . args] _]
     (pred dot #(= % '.))
     (ann-instancemethod ctx name method args type))
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

(check '(fn [s] (Class/forName s)))

(check '(fn [^String s] (.endsWith s ".clj")))

(check '(.contains "clojure" "j"))

(first (check '(fn [^String s c] (.contains s c))))
