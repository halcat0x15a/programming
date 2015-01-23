---
layout: default
title: Monad in Clojure
---

# Monad in Clojure

Clojureでモナドを表現するための方法をいくつか紹介する.

## モナド構文

モナドを扱うための構文は2種類存在する.

1つはHaskellのdoやF#のcomputationであり, `bind`を必要とする.

もう1つはScalaのforやHaskellの内包表記であり, `bind`と`fmap`を必要とする.また, ガードをサポートするならば`filter`が必要になる.

do記法は次のような変換規則をもつ.

```clojure
(= (do-m
     (let! a ma)
     (return (f a)))
   (bind ma (fn [a] (return (f a)))))
(= (do-m
     (let! a ma)
     (let! b mb)
     (return (f a b)))
   (bind ma (fn [a] (bind mb (fn [b] (return (f a b)))))))
(= (do-m
     (let! a ma)
     mb
     (return (f a)))
   (bind ma (fn [a] (bind mb (fn [_] (return (f a)))))))
```

一方, 内包表記の変換規則は次のようになる.

```clojure
(= (for-m [a ma] (f a))
   (fmap ma (fn [a] (f a))))
(= (for-m [a ma b mb] (f a b))
   (bind ma (fn [a] (fmap mb (fn [b] (f a b))))))
(= (for-m [a ma b mb c mc] (f a b c))
   (bind ma (fn [a] (bind mb (fn [b] (fmap mc (fn [c] (f a b c))))))))
```

これらはマクロを使えば簡単に記述できる.

```clojure
(defmacro do-m [[tag var val :as expr] & exprs]
  (cond (empty? exprs) expr
        (= tag 'let!) `(bind ~val (fn [~var] (do-m ~@exprs)))
        :else `(bind ~expr (fn [_#] (do-m ~@exprs)))))

(defmacro for-m [[var val & exprs] expr]
  (if exprs
    `(bind ~val (fn [~var] (for-m ~exprs ~expr)))
    `(fmap ~val (fn [~var] ~expr))))
```

do記法は内包表記に比べて`return`を書く必要性が高い.

`return`は`bind`や`fmap`に比べて実装の自由度が低いのでこの記事では内包表記風の構文を採用する.

## 多相性

`fmap`, `bind`は多相的な関数でなければならない.

多相的な関数を実現するためにはいくつかの方法が考えられる.

* multimethod
* protocol
* metadata
* dynamic

### multimethod

型によるディスパッチを行う.

```clojure
(defmulti fmap (fn [m f] (class m)))

(defmulti bind (fn [m f] (class m)))

(defmethod fmap clojure.lang.ISeq [m f] (map f m))

(defmethod bind clojure.lang.ISeq [m f] (mapcat f m))

(is (= (for-m [x '(1 2 3)
               y (list x x)]
         (inc y))
       '(2 2 3 3 4 4)))
```

### protocol

multimethodよりも効率的に動作する.

```clojure
(defprotocol Monad
  (fmap [m f])
  (bind [m f]))

(extend-protocol Monad
  clojure.lang.ISeq
  (fmap [m f] (map f m))
  (bind [m f] (mapcat f m)))

(is (= (for-m [x '(1 2 3)
               y (list x x)]
         (inc y))
       '(2 2 3 3 4 4)))
```

### metadata

オブジェクトにメタデータを付与することで多相性が実現できる.

```clojure
(defn fmap [m f]
  (vary-meta ((:fmap (meta m)) f m) merge (meta m)))

(defn bind [m f]
  (vary-meta ((:bind (meta m)) f m) merge (meta m)))

(defn point-seq [s] (with-meta s {:fmap map :bind mapcat}))

(is (= (for-m [x (point-seq '(1 2 3))
               y (point-seq (list x x))]
         (inc y))
       '(2 2 3 3 4 4)))
```

メタデータを付与できるのはIObjを継承しているクラスだけである.

### dynamic

動的束縛によりコンテキストを外部から導入することができる.

```clojure
(def ^:dynamic *monad*)

(defn fmap [m f] ((:fmap *monad*) f m))

(defn bind [m f] ((:bind *monad*) f m))

(def seq-monad
  {:fmap map
   :bind mapcat})

(binding [*monad* seq-monad]
  (is (= (for-m [x '(1 2 3)
                 y (list x x)]
           (inc y))
         '(2 2 3 3 4 4))))
```

dynamicによる実装は次のような利点がある.

* `return`による実装が可能
*  1つのデータ型に対して複数のモナドを実装できる

しかし, コンテキストを明示的に指定する必要があるためprotocolにくらべて冗長になる.
