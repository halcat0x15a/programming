---
layout: default
title: Lisp on Clojure
---

# Lisp on Clojure

Clojureの特徴を簡単なLisp評価機を作る過程で紹介します.

## 評価機

この評価機は,以下のような動作をします.

```clojure
(def env (atom {'+ +}))

(eval env '(define double (lambda (x) (+ x x))))

(eval env '(define foo (double (double 3))))

(assert (= (eval env 'foo) 12))
```

この例は`env`の元で関数`double`と`foo`を定義し,`assert`により`foo`の値が12であることを確かめています.

### Atom

Clojureは`def`により`var`を`namespace`に束縛しています.

しかし,通常他のLispのように`set!`による代入は出来ません.

ここでは,可変参照として`atom`を使用します.

```clojure
(def foo (atom 1))

(assert (= @foo 1))

(reset! foo 2)

(assert (= @foo 2))

(swap! foo inc)

(assert (= @foo 3))
```

`@`は`deref`の呼び出しであり,Atomの現在の値を返します.

## 自己評価式

文字列や数値などを評価します.

```clojure
(defn self-evaluating? [exp]
  (or (true? exp)
      (false? exp)
      (number? exp)
      (string? exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp))

(assert (= (eval (atom {}) 100) 100))
(assert (= (eval (atom {}) "foo") "foo"))
```

## 変数の探索

環境は`hash-map`で表現しています

```clojure
(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (@env exp)))

(assert (= (eval (atom {'foo 100}) 'foo)) 100)
```

## 特殊形式

特殊形式の判別はリストの先頭を比較する必要があります.

ClojureのMultimethodを用いて,`eval`を変更することなく制御構造を追加します.

```clojure
(defmulti eval-form (fn [env exp] (first exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (@env exp)
        (seq? exp) (eval-form env exp)))
```

### Multimethod

`defmulti`でディスパッチ関数を定義し,`defmethod`により対応する値と手続きを記述します.

単純な比較ではなく,階層も考慮されます.

```clojure
(defmulti foo (fn [x] x))
(defmethod foo 'foo [x] 100)
(defmethod foo 'bar [x] "bar")
(defmethod foo :default [x] x)

(assert (= (foo 'foo) 100))
(assert (= (foo 'bar) "bar"))
(assert (= (foo 'baz) 'baz))
```

## quote, if

式の分解には分配束縛を用います.

```clojure
(defmethod eval-form 'quote [env [_ quotation]] quotation)

(defmethod eval-form 'if [env [_ predicate consequent alternative]]
  (if (eval env predicate)
    (eval env consequent)
    (eval env alternative)))

(assert (= (eval (atom {}) '(quote (foo bar))) '(foo bar)))
(assert (= (eval (atom {}) '(if false "foo" 100)) 100))
```

## define

`swap!`によりAtomの値を書き換えます.

Clojureでは無名関数(fn [x] (f x))を#(f %)と記述できます.

```clojure
(defmethod eval-form 'define [env [_ variable value]]
  (swap! env #(assoc % variable (eval env value))))

(def env (atom {}))
(eval env '(define foo 100))
(assert (= (eval env 'foo) 100))
```

## begin

`&`を使うことで複数の値を束縛出来ます.

```clojure
(defmethod eval-form 'begin [env [& exps]]
  (->> exps (map (partial eval env)) last))

(assert (= (eval (atom {}) '(begin (define bar "bar") bar)) "bar"))
```

### Arrow

`->`や`->>`はArrowと呼ばれ,以下のような利点があります.

* データの流れが分り易い
* ネストが無くなる
* 括弧が減る

```clojure
(assert (= (-> [] (conj "foo") (conj "bar") first)
           (first (conj (conj [] "foo") "bar"))))

(assert (= (->> (range 10) (filter odd?) reverse)
           (reverse (filter odd? (range 10)))))
```

## 適用

この評価機では２種類の関数が存在します.

* 評価機で定義した関数
* Clojureの関数

これらに対してProtocolを定義します.

```clojure
(defprotocol Procedure
  (appl [f args]))

(defmethod eval-form :default [env [operator & operands]]
  (appl (eval env operator) (map (partial eval env) operands)))
```

### Protocol

ProtocolはClojureにおける抽象機構であり,Javaのinterfaceと似ています.

TypeやRecordは定義時にProtocolを実装することができます.

```clojure
(defprotocol Foo
  (foo [x]))

(deftype Bar [value]
  Foo
  (foo [x] value))

(defrecord Baz []
  Foo
  (foo [x] "baz"))

(assert (= (foo (Bar. 100)) 100))
(assert (= (foo (Bar. "bar")) "bar"))
(assert (= (foo (Baz.)) "baz"))
```

## lambda

仮引数と実引数のペアで環境を拡張します.

```clojure
(deftype Lambda [env parameters body]
  Procedure
  (appl [lambda args]
    (eval (atom (merge @env (zipmap parameters args))) body)))

(defmethod eval-form 'lambda [env [_ parameters body]]
  (Lambda. env parameters body))

(assert (= (eval (atom {}) '((lambda (x y) y) "foo" "bar")) "bar"))
```

## primitive

extend-protocolにより既存のデータ型に対してProtocolの実装が可能です.

```clojure
(import 'clojure.lang.IFn)

(extend-protocol Procedure
  IFn
  (appl [f args] (apply f args)))

(assert (= (eval (atom {'+ +}) '(+ 2 3)) 5))
```

これで目標とする評価機が完成しました.

## ClojureScript

Clojureのみで書かれているので,JavaScriptにコンパイルが可能…

な筈ですが`extend-protocol`の部分を書き換える必要があります.

```clojure
(extend-protocol Procedure
  js/Function
  (appl [f args] (apply f args)))
```

以下に今回実装した評価機を示します.

<div>
  <div class="form-group">
    <label for="result" class="control-label">Result</label>
    <input type="text" class="form-control" id="result" readonly>
  </div>
  <textarea id="source" class="form-control"></textarea>
  <button class="btn btn-default" onclick="document.getElementById('result').value = evaluator.run(document.getElementById('source').value)">Eval</button>
</div>

<script src="target/evaluator.js" type="text/javascript"></script>
