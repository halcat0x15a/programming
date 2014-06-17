---
layout: default
title: type inference with logic programming
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

`[[sym type] & pairs]`

型環境は変数と型のペアのベクタで表現されます.

型環境から型を導出する規則は次のようになります.

```clojure
(defna ann-var [ctx sym type]
  ([[[sym type] . _] _ _])
  ([[_ . ctx'] _ _] (ann-var ctx' sym type)))
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

## 特殊形式

制御構造の最小単位として特殊形式が存在します.

`clojure.walk/macroexpand-all`を使って,特殊形式を使った形に展開しましょう.

```clojure
(defn check
  ([expr] (check [] expr))
  ([ctx expr]
    (run* [type] (ann ctx (macroexpand-all expr) type))))
```

特殊形式に対する型付けを考えてみましょう.

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

(check '(do 0 [] "foo")) ; => (java.lang.String)

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
  ([_ ['fn* [syms . exprs]] [::fn return . params]]
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

`(fn [a] a)`という式に対して,この型推論器は`[::fn _0 _0]`という型付けを行います.

`core.logic`は未束縛の論理変数に対して`_0`,`_1`といった風にインデックスを付けます.

この関数に対して文字列を適用する式は`((fn [a] a) "")`と書くことができ,これは`java.lang.String`に推論されます.

つまり,未束縛の論理変数は型システムにおける型変数とみなすことができます.

## Type tagによる型の明示

現在の関数の型付け規則では多相的な関数しか定義できません.

そこで,`tag`により型を推論する規則を加えます.

```clojure
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

(check '(fn [^String s] s)) ; => ([::fn java.lang.String java.lang.String])

(check '((fn [^String s] s) 0)) ; => ()
```

単一化に失敗すると結果は得られず,型エラーとなります.

## 名前付き関数と再帰

Clojureでは無名関数に名前を付けることで,再帰関数を定義することができます.

名前付き関数に対する規則を`ann`に追加しましょう.

型環境に自身を加えることで再帰することができます.

```clojure
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

(check '(fn f [a] (if true a (f "")))) ; => ([::fn java.lang.String java.lang.String])
```

## メソッド

`.`はクラスメソッドやインスタンスメソッドの呼び出しを行うための特殊形式です.

メソッドはクラスからリフレクションにより取得可能です.

```clojure
(defn methods [^Class class method]
  (->> (.getMethods class)
       (filter #(= (.getName ^Method %) (name method)))
       (map (fn [ ^Method m] (cons (.getReturnType m) (.getParameterTypes m))))))
```

メソッドはオーバーロードされている可能性があるため,複数の型を単一化する規則を与えます.

```clojure
(defne ann-call [ctx methods args type]
  ([_ [[type . params] . _] _ _]
     (ann-list ctx args params))
  ([_ [_ . methods'] _ _]
     (ann-call ctx methods' args type)))
```

クラスメソッドは`name`を解決して得た型から取得されます.

```clojure
(defn ann-classmethod [ctx name method args type]
  (fresh [class]
    (pred name symbol?)
    (is class name resolve)
    (pred class class?)
    (project [class method]
      (ann-call ctx (methods class method) args type))))
```

インスタンスメソッドは`expr`を評価して得た型から取得されます.

```clojure
(defn ann-instancemethod [ctx expr method args type]
  (fresh [class]
    (ann ctx expr class)
    (project [class method]
      (ann-call ctx (methods class method) args type))))
```

`ann`をメソッド呼び出しに対応させると次のようになります.

```clojure
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

(check '(fn [s] (Class/forName s))) ; => ([::fn java.lang.Class java.lang.String])

(check '(fn [^String s] (.endsWith s ".clj"))) ; => ([::fn boolean java.lang.String])
```

## サブタイピング

さて,メソッドの型を推論できるようになりましたがここで大きな問題が見えてきました.

次の式は実行可能ですが推論できません.

```clojure
(.contains "clojure" "j")

(check '(.contains "clojure" "j")) ; => ()
```

`.contains`の型を見てみましょう.

```clojure
(check '(fn [^String s c] (.contains s c))) ; => ([::fn boolean java.lang.String java.lang.CharSequence])
```

`java.lang.String`は`java.lang.CharSequence`のサブクラスですが,単純な単一化ではis-a関係を考慮することはできません.

そこで,論理変数に対してあるクラスのサブタイプであることを制約として記述します.

```clojure
(defna subtypes [xs ys]
  ([[] []])
  ([[x . xs'] [y . ys']]
     (project [y]
       (predc x #(isa? % y) (fn [_ _ r s] `(isa? ~(-reify s x r) ~y))))))
```

`subtypes`は単一化の時に`isa?`を検査する制約です.

これを,`ann-app`と`ann-call`に組み込みましょう.

```clojure
(defn ann-app [ctx expr type]
  (fresh [types]
    (ann-list ctx expr types)
    (matcha [types]
      ([[[::fn type . params] . args]]
         (subtypes args params)))))

(defne ann-call [ctx methods args type]
  ([_ [[type . params] . _] _ _]
     (fresh [args']
       (ann-list ctx args args')
       (subtypes args' params)))
  ([_ [_ . methods'] _ _]
     (ann-call ctx methods' args type)))
```

この修正により`.contains`の式に型が付くようになりました.

```clojure
(check '(.contains "clojure" "j")) ; => (boolean)
```

`.contains`には次のような型が付きます

```clojure
(check '(fn [^String s c] (.contains s c))) ; => (([::fn boolean java.lang.String _0] :- (isa? _0 java.lang.CharSequence)))
```

## オーバーロード

Clojureでは引数の数でオーバーロードされた関数を定義することが可能です.

再帰とか考えると[むずかしい](https://gist.github.com/halcat0x15a/173b647b247221c9a1dd).
