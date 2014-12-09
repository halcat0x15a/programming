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

## チャネルの選択

`alts!!`は複数のチャネルの内,書き込みが行われたチャネルと値のペアを返します.

```clojure
(require '[clojure.core.async :refer [chan >!! alts!!]])

(let [c1 (chan)
      c2 (chan)]
  (future (>!! c1 :foo)
          (>!! c2 :bar))
  (assert (= (alts!! [c1 c2]) [:foo c1]))
  (assert (= (alts!! [c1 c2]) [:bar c2])))
```

`alt!!`は複数のチャネルの操作から一つ選択します.

```clojure
(require '[clojure.core.async :refer [chan >!! alt!!]])

(let [c1 (chan)
      c2 (chan)]
  (future (>!! c1 :foo)
          (>!! c2 :bar))
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

10000ms後に閉じられるチャネルと100ms後に閉じられるチャネルでは,後者の方が早くに閉じられます.

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

`go`は`<!`や`>!`の呼び出しを見つけ出し,SSA形式のステートマシンを生成します.

継続はcore.asyncのスレッドプールから生成されるスレッドで処理されます.

```clojure
(require '[clojure.core.async :refer [chan timeout go <! >! <!!]])

(let [c (chan)]
  (dotimes [_ 100]
    (go
      (let [a (.getId (Thread/currentThread))]
        (<! (timeout 1000))
        (let [b (.getId (Thread/currentThread))]
          (>! c [a b])))))
  (->> (range 100)
       (map (fn [_] (<!! c)))
       (filter #(apply distinct? %))))

; ([48 55] [46 42] [49 61] [45 91] [43 46] [42 45] [50 49] [51 45] [52 49] [53 56] [54 49] [55 56] [56 49] [59 56] [60 49] [58 56] [57 49] [61 56] [62 49] [63 47] [64 47] [65 47] [66 43] [67 90] [68 43] [69 90] [70 55] [71 43] [72 90] [73 55] [74 90] [75 57] [76 54] [77 57] [78 54] [79 63] [80 49] [81 63] [82 65] [83 68] [84 68] [85 68] [86 47] [88 47] [87 68] [89 47] [90 68] [91 47] [43 68] [43 47] [44 68] [43 47] [44 68] [91 47] [49 68] [55 47] [49 79] [56 79] [59 79] [60 79] [50 79] [60 76] [50 79] [52 76] [58 79] [57 76] [54 79] [48 43] [48 43] [63 43] [64 43] [63 55] [65 43] [64 55] [66 43] [67 55] [70 43] [71 55] [70 43] [74 55] [73 43] [73 55] [77 43] [78 55] [73 90] [79 55] [79 42] [79 61] [79 42] [84 61] [85 42] [84 61] [85 42] [89 61] [87 91] [79 43] [43 62] [90 62])
```

この例では`go`の内側が別々のスレッドで処理されていることを示しています.

スレッドプールのサイズは`プロセッサ数 * 2 + 42`.

```clojure
(require '[clojure.core.async :refer [chan >! <!! >!!]])

; future
(time
  (let [c (chan)]
    (dotimes [n 100]
      (future
        (Thread/sleep 1000)
        (>!! c (.getId (Thread/currentThread)))))
    (count (distinct (map (fn [_] (<!! c)) (range 100))))))
; "Elapsed time: 1270.819965 msecs"
; => 100
 
; go
(time
  (let [c (chan)]
    (dotimes [n 100]
      (go
        (Thread/sleep 1000)
        (>! c (.getId (Thread/currentThread)))))
    (count (distinct (map (fn [_] (<!! c)) (range 100))))))
; "Elapsed time: 2012.681518 msecs"
; => 50
```
