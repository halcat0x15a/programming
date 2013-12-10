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

上記のコードでは,`future`で非同期にチャネルから値を読み込み,チャネルに値を書き込んだ後,`future`から値を取得します.

また,`timeout`は指定した時間の後閉じられるチャネルを返します.

```clojure
(require '[clojure.core.async :refer (timeout <!!)])

(assert (= (<!! (timeout 1000)) nil))
```

実行すると1秒後に値が返ることでしょう.

## go

`go`はcore.asyncが提供する非同期実行の仕組みです.

`go`の内側では`>!!`と`<!!`の代わりに,`>!`と`<!`を用います.

```clojure
(require '[clojure.core.async :refer (chan <! >! go)])

(let [c (chan)]
  (go (loop [n 0] (>! c n) (recur (inc n))))
  (go (assert (= (<! c) 0))
      (assert (= (<! c) 1))
      (assert (= (<! c) 2))))
```

チャネルから値を読み出す毎に値がインクリメントされていることが確認できます.

`go`は`<!`や`>!`の呼び出しを見つけ出し,ステートマシンを生成します.

`go`の内側でマクロや動的束縛を使うとうまく動かないことがあるので注意しましょう.

## チャネルの選択

`alts!!`は複数のチャネルの内,書き込みが行われたチャネルと値のペアを返します.

```clojure
(require '[clojure.core.async :refer (chan >! alts!! go)])

(let [c1 (chan)
      c2 (chan)]
  (go (>! c1 :foo))
  (assert (= [:foo c1] (alts!! [c1 c2])))
  (go (>! c2 :bar))
  (assert (= [:bar c2] (alts!! [c1 c2]))))
```

`alt!!`は複数のチャネルの操作から一つ選択します.

```clojure
(require '[clojure.core.async :refer (chan >! alt!! go)])

(let [c1 (chan)
      c2 (chan)]
  (go (>! c1 :foo))
  (alt!! c1 ([v] (assert (= v :foo)))
         c2 ([v] (assert false)))
  (go (>! c2 :bar))
  (alt!! c1 ([v] (assert false))
         c2 ([v] (assert (= v :bar)))))
```

これらの関数は`timeout`と組み合わせると有用です.

```clojure
(require '[clojure.core.async :refer (timeout alt!!)])

(assert (= (alt!! (timeout 10000) ([] :foo)
                  (timeout 100) ([] :bar))
           :bar))
```

10000ms後に閉じられるチャネルと100ms後に閉じられるチャネルだと,後者の方が早くに閉じられます.

## サンプル

簡単なサンプルプログラムを見てみましょう.

clojuredocsのAPIを使って複数のライブラリから関数を検索します.

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

`-main`は単純で,

* 標準入力からクエリを取得
* `go`の内側で非同期に通信
* `alt!`を使って通信結果かタイムアウトか選択

といったことを行っています.

以下は実行例です.

```clojure
search=> clojure.core/map
search=> [{"id" 1494,
  "name" "sorted-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1494"}
 {"id" 1577,
  "name" "ns-unmap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1577"}
 {"id" 1579,
  "name" "zipmap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1579"}
 {"id" 1734,
  "name" "map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1734"}
 {"id" 1756,
  "name" "mapcat",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1756"}
 {"id" 1783,
  "name" "sorted-map-by",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1783"}
 {"id" 1835,
  "name" "map?",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1835"}
 {"id" 1871,
  "name" "amap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1871"}
 {"id" 1901,
  "name" "struct-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1901"}
 {"id" 1904,
  "name" "proxy-mappings",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1904"}
 {"id" 1952,
  "name" "pmap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1952"}
 {"id" 1974,
  "name" "map-indexed",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1974"}
 {"id" 1997,
  "name" "ns-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1997"}
 {"id" 2012,
  "name" "array-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/2012"}
 {"id" 2064,
  "name" "hash-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/2064"}
 {"id" 5350,
  "name" "amap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5350"}
 {"id" 5355,
  "name" "array-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5355"}
 {"id" 5544,
  "name" "hash-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5544"}
 {"id" 5600,
  "name" "map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5600"}
 {"id" 5601,
  "name" "map-indexed",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5601"}
 {"id" 5602,
  "name" "map?",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5602"}
 {"id" 5603,
  "name" "mapcat",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5603"}
 {"id" 5635,
  "name" "ns-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5635"}
 {"id" 5641,
  "name" "ns-unmap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5641"}
 {"id" 5659,
  "name" "pmap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5659"}
 {"id" 5683,
  "name" "proxy-mappings",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5683"}
 {"id" 5763,
  "name" "sorted-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5763"}
 {"id" 5764,
  "name" "sorted-map-by",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5764"}
 {"id" 5775,
  "name" "struct-map",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5775"}
 {"id" 5854,
  "name" "zipmap",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5854"}]
search=> filter
search=> [{"id" 1309,
  "name" "stream-filter",
  "ns" "clojure.contrib.stream-utils",
  "url" "http://clojuredocs.org/v/1309"}
 {"id" 1784,
  "name" "filter",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/1784"}
 {"id" 3018,
  "name" "filter-kind",
  "ns" "incanter.processing",
  "url" "http://clojuredocs.org/v/3018"}
 {"id" 3496,
  "name" "ant-filter",
  "ns" "lancet",
  "url" "http://clojuredocs.org/v/3496"}
 {"id" 4156,
  "name"
  "swank.util.class_browse.proxy$java.lang.Object$FilenameFilter$d73a157b",
  "ns" "swank.util.class-browse",
  "url" "http://clojuredocs.org/v/4156"}
 {"id" 5504,
  "name" "filter",
  "ns" "clojure.core",
  "url" "http://clojuredocs.org/v/5504"}]
search=> 
```

data.jsonについては[data.jsonでJSONの読み書き](http://athos.hatenablog.com/entry/dealing_with_JSON_using_data_json)を参照すると良いでしょう.

## まとめ