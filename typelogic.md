---
layout: default
title: type inference and logic programming
---

# 型推論と論理プログラミング

core.logicを用いてClojureの型推論器を実装します.

## 単相型

自己評価可能な値に対しての型付けを考えてみましょう.

ClojureはJVMで動作するため, Javaのリフレクションにより値から型を取得することが可能です.

```clojure
(class "hoge") ; => java.lang.String

(class []) ; => clojure.lang.PersistentVector
```

型付け規則を`core.logic`で記述していきます.

`ann`は式と型を対応させます.

```clojure
(defna ann [expr type]
  ([_ _] (is type expr class)))
```

`check`は`ann`を使って式から型を得ます.

```clojure
(defn check [expr]
  (run* [type] (ann expr type)))

(check "hoge") ; => (java.lang.String)
```

## 型環境

変数と型を結びつけるために型環境を導入します.

`[[a a-type] [b b-type]]`

型環境は変数と型のペアのベクタで表現されます.

型環境から型を導出する規則は次のようになります.

```clojure
(defna ann-var [ctx expr type]
  ([[[expr type] . _] _ _])
  ([[_ . ctx'] _ _] (ann-var ctx' expr type)))
```

これを使って`ann`を拡張すると次のようになります.

```clojure
(defna ann [ctx expr type]
  ([_ _ _] (ann-var ctx expr type))
  ([_ _ _] (is type expr class)))

(defn check
  ([expr] (check [] expr))
  ([ctx expr]
    (run* [type] (ann ctx expr type))))

(check [['a java.lang.String]] 'a) ; => (java.lang.String)
```

## special form

制御構造に対する型付けを考えてみましょう.

`do`は複数の式を順に評価して,最後の式を結果とします.

```clojure
(defna ann-do [ctx exprs type]
  ([_ [expr] _] (ann ctx expr type))
  ([_ [expr . exprs'] _]
    (fresh [type] (ann ctx expr type))
    (ann-do ctx exprs' type)))
```

`if`は`test`が真のときに`consequent`が,偽のときに`alternative`が評価されます.

この型推論器では`consequent`と`alternative`の型が一致するものとします.

```clojure
(defn ann-if [ctx test consequent alternative type]
  (all
    (fresh [type] (ann ctx test type))
    (ann ctx consequent type)
    (ann ctx alternative type)))
```

この2つの規則で`ann`を拡張しましょう.

```clojure
(defna ann [ctx expr type]
  ([_ ['do . exprs] _]
    (ann-do ctx exprs type))
  ([_ ['if test consequent alternative] _]
    (ann-if ctx test consequent alternative type))
  ([_ _ _] (ann-var ctx expr type))
  ([_ _ _] (is type expr class)))

(check '(if true ["hoge"] [])) ; => (clojure.lang.PersistentVector)
```

## 関数と適用

Clojureにおける関数の型は`clojure.lang.IFn`ですが, これではどのような型を引数にとり, どのような型を返すのかわかりません.

そこで,この型推論器の独自の型を定義します.

`[::fn return-type . parameter-types]`

関数はパラメータと式から成り立ち,式はパラメータにより拡張された型環境の中で推論が行われます.

```clojure
(defna ann-fn [ctx syms exprs params return]
  ([_ [] _ [] _] (ann-do ctx exprs return))
  ([_ [sym . syms'] _ [param . params'] _]
   (fresh [ctx']
     (conso [sym param] ctx ctx')
     (ann-fn ctx' syms' exprs params' return))))
```

また,この型を使った適用の規則は次のようになります.

```clojure
(defna ann-list [ctx exprs types]
  ([_ [] []])
  ([_ [expr . exprs'] [type . types']]
    (ann ctx expr type)
    (ann-list ctx exprs' types')))

(defn ann-app [ctx expr type]
  (fresh [types]
    (ann-list ctx expr types)
    (matcha [types] ([[[::fn type . params] . params]]))))
```

`ann`に組み込んでみましょう.

```clojure
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

(check '(fn [a] a)) ; => ([::fn _0 _0])

(check '((fn [a] a) "foo")) ; => (java.lang.String)
```


## 論理変数と型変数

