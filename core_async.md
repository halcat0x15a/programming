# core.async

Clojureにおける非同期プログラミングの紹介

# channel

core.asyncではチャネルを用いて値をやりとりする.

チャネルの生成には`chan`を使う.

チャネルに値を書き込むには`>!!`,読み込むには`<!!`を使う.

これらの関数は同期的に実行され,操作が完了するまでスレッドはブロックされる.

通常,`future`などと組み合わせて使う.

```clojure
(require '[clojure.core.async :refer (chan <!! >!!)])

(let [c (chan)
      r (future (<!! c))]
  (>!! c "hello")
  (assert (= @r "hello")))
```

# go

core.asyncが提供する非同期実行の仕組み.

`>!!`と`<!!`の代わりに,`>!`と`<!`を用いる.

```clojure
(require '[clojure.core.async :refer (chan <! >! go go-loop)])

(let [c (chan)]
  (go-loop [n 0] (>! c n) (recur (inc n)))
  (go (<! c)
      (<! c)
      (assert (= (<! c) 2))))
```
