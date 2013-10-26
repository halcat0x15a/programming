# core.async

# channel

core.asyncではchannelを用いて値をやりとりする.

channelの生成には`chan`を使う.

channelに値を書き込むには`>!!`,読み込むには`<!!`を使う.

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
  (go (assert (= (<! c) 0))
      (assert (= (<! c) 1))
      (assert (= (<! c) 2))))
```

# alt!

`alts!`は複数のchannelの内,書き込みが行われたchannelと値のペアを返す.

```clojure
(require '[clojure.core.async :refer (chan >! <!! >!! alts! go)])

(let [c1 (chan)
      c2 (chan)
      out (chan)
      from (chan)]
  (go (while true
        (let [[v c] (alts! [c1 c2])]
          (>! out v)
          (>! from c))))
  (>!! c1 0)
  (assert (= (<!! out) 0))
  (assert (= (<!! from) c1))
  (>!! c2 1)
  (assert (= (<!! out) 1))
  (assert (= (<!! from) c2)))
```

`alt!`は複数のchannelの操作から一つ選択する.

```clojure
```

# ops

channelのtarget,sourceに対して`map`や`filter`を適用する.

```clojure
(require '[clojure.core.async :refer (chan <! >! filter> map< go go-loop)])

(let [c (->> (chan)
             (filter> odd?)
             (map< #(* % %)))]
  (go-loop [n 0] (>! c n) (recur (inc n)))
  (go (assert (= (<! c) 1))
      (assert (= (<! c) 9))
      (assert (= (<! c) 25))))
```

channel同士をpipeでつなぐ.

```clojure
(require '[clojure.core.async :refer (chan <! >! pipe go)])

(let [in (chan) out (chan)]
  (go (dotimes [n 10] (>! in n)))
  (go (while true (prn (<! out))))
  (pipe in out))
```
