---
layout: default
title: core.async
---

# core.async

core.asyncはその名の通り非同期プログラミングをサポートします.

## インストール

この記事に記載されるコードは以下の環境で動作します.

```clojure
(defproject programming "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]])
```

## チャネル

core.asyncではチャネルを用いて値をやりとりします.

チャネルの生成には`chan`を使い,チャネルに値を書き込むには`>!!`,読み込むには`<!!`を使います.

これらの関数は操作が完了するまでスレッドをブロックすることから,通常,`future`などと組み合わせて使います.

```clojure
(require '[clojure.core.async :refer [chan <!! >!!]])

(let [c (chan)
      r (future (<!! c))]
  (>!! c "hello")
  (assert (= @r "hello")))
```

上記のコードでは,`future`で非同期にチャネルから値を読み込み,チャネルに値を書き込んだ後,`future`から値を取得します.

また,`timeout`は指定した時間の後閉じられるチャネルを返します.

```clojure
(require '[clojure.core.async :refer [timeout <!!]])

(assert (= (<!! (timeout 1000)) nil))
```

実行すると1秒後に値が返ります.

## go

`go`はcore.asyncが提供する非同期実行の仕組みです.

`go`の内側では`>!!`と`<!!`の代わりに,`>!`と`<!`を用います.

```clojure
(require '[clojure.core.async :refer [chan <! >! go]])

(let [c (chan)]
  (go (loop [n 0]
        (>! c n)
        (recur (inc n))))
  (go (assert (= (<! c) 0))
      (assert (= (<! c) 1))
      (assert (= (<! c) 2))))
```

チャネルから値を読み出す毎に値がインクリメントされていることが確認できます.

`go`は`<!`や`>!`の呼び出しを見つけ出し,ステートマシンを生成します.

## チャネルの選択

`alts!!`は複数のチャネルの内,書き込みが行われたチャネルと値のペアを返します.

```clojure
(require '[clojure.core.async :refer [chan >! alts!! go]])

(let [c1 (chan)
      c2 (chan)]
  (go (>! c1 :foo)
      (>! c2 :bar))
  (assert (= (alts!! [c1 c2]) [:foo c1]))
  (assert (= (alts!! [c1 c2]) [:bar c2])))
```

`alt!!`は複数のチャネルの操作から一つ選択します.

```clojure
(require '[clojure.core.async :refer [chan >! alt!! go]])

(let [c1 (chan)
      c2 (chan)]
  (go (>! c1 :foo)
      (>! c2 :bar))
  (alt!! c1 ([v] (assert (= v :foo)))
         c2 ([v] (assert false)))
  (alt!! c1 ([v] (assert false))
         c2 ([v] (assert (= v :bar)))))
```

これらの関数は`timeout`と組み合わせると有用です.

```clojure
(require '[clojure.core.async :refer [timeout alt!!]])

(assert (= (alt!! (timeout 10000) ([] :foo)
                  (timeout 100) ([] :bar))
           :bar))
```

10000ms後に閉じられるチャネルと100ms後に閉じられるチャネルだと,後者の方が早くに閉じられます.
