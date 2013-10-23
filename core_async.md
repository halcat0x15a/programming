# core.async

Clojureにおける非同期プログラミングの紹介

# channel

core.asyncではチャネルを用いて値をやりとりする.

チャネルの生成にはchanを使う.

チャネルに値を送るには>!!,取得するには<!!を使う.

これらの関数は同期的に実行され,操作が完了するまでスレッドはブロックされる.

通常,futureなどと組み合わせて使う.

```clojure
(let [c (chan)
      r (future (<!! c))]
  (>!! c "hello")
  (assert (= @r "hello")))
```
