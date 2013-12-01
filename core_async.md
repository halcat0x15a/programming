---
layout: default
title: core.async
---

# core.async

core.asyncはその名の通り非同期プログラミングをサポートします.

この記事では主な使い方とサンプルを紹介していきます.

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
(require '[clojure.core.async :refer (chan <!! >!!)])

(let [c (chan)
      r (future (<!! c))]
  (>!! c "hello")
  (assert (= @r "hello")))
```

`timeout`は指定した時間の後閉じられるチャネルを返します.

```clojure
(require '[clojure.core.async :refer (timeout <!!)])

(assert (= (<!! (timeout 1000)) nil))
```

## go

core.asyncが提供する非同期実行の仕組みです.

`go`の内側では`>!!`と`<!!`の代わりに,`>!`と`<!`を用います.

```clojure
(require '[clojure.core.async :refer (chan <! >! go go-loop)])

(let [c (chan)]
  (go-loop [n 0] (>! c n) (recur (inc n)))
  (go (assert (= (<! c) 0))
      (assert (= (<! c) 1))
      (assert (= (<! c) 2))))
```

`go-loop`は単なる(go (loop ...))の糖衣構文です.

core.asyncで定義される関数は,`go`の内側では!,外側では!!というsuffixが付くと覚えておけば良いでしょう.

`go`は`<!`や`>!`の呼び出しを見つけ出し,ステートマシンを生成します.

`go`の内側で動的束縛やSTMなどを使うと,うまく動かない事があるので注意しましょう.

## チャネルの選択

`alts!`は複数のチャネルの内,書き込みが行われたチャネルと値のペアを返します.

```clojure
(require '[clojure.core.async :refer (chan >! <!! >!! alts! go)])

(let [c1 (chan)
      c2 (chan)]
  (go (while true
        (let [[v c] (alts! [c1 c2])]
          (>! c (inc v)))))
  (go (>! c1 0)
      (assert (= (<! c1) 1)))
  (go (>! c2 2)
      (assert (= (<! c2) 3))))
```

`alt!`は複数のチャネルの操作から一つ選択します.

```clojure
(require '[clojure.core.async :refer (chan >! <! alt! go)])

(let [c1 (chan)
      c2 (chan)]
  (go (while true
        (alt! c1 ([v c] (>! c (inc v)))
              c2 ([v c] (>! c (dec v))))))
  (go (>! c1 0)
      (assert (= (<! c1) 1)))
  (go (>! c2 0)
      (assert (= (<! c2) -1))))
```

これらの関数は`timeout`と組み合わせると有用です.

```clojure
(require '[clojure.core.async :refer (chan timeout >! <!! alt! go)])

(let [c (chan)]
  (go (alt! (timeout 10000) ([] (>! c "foo"))
            (timeout 100) ([] (>! c "bar"))))
  (assert (= (<!! c) "bar")))
```

## サンプル

簡単なサンプルプログラムを見てみましょう.

clojuredocsのAPIを使って,複数のライブラリから関数を検索します.

```clojure
(ns clojuredocs-search
  (:require [clojure.pprint :refer (pprint)]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.core.async :refer (chan timeout go >! <! alt! >!!)]))

(def deadline 3000)

(defn search [query]
  (-> (str "http://api.clojuredocs.org/search/" query)
      io/as-url
      io/reader
      json/read))

(defn prompt []
  (print "search=> ")
  (flush))

(defn -main []
  (let [query (chan)]
    (go (while true
          (let [query (<! query)
                result (chan)]
            (go (>! result (or (search query) [])))
            (alt! result ([v] (pprint v))
                  (timeout deadline) ([] (println "timeout")))
            (prompt))))
    (while true
      (prompt)
      (>!! query (read-line)))))
```

標準入力からクエリを取得し,`go`の内側で非同期に通信します.

`alt!`を使って通信結果が返った時とタイムアウトした時で場合分けしています.

data.jsonについては[data.jsonでJSONの読み書き](http://athos.hatenablog.com/entry/dealing_with_JSON_using_data_json)を参照すると良いでしょう.
