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

`timeout`は指定した時間の後閉じられるchannelを返す.

```clojure
(require '[clojure.core.async :refer (timeout <!!)])

(assert (= (<!! (timeout 1000)) nil))
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
      r (chan)]
  (go (while true (>! r (alts! [c1 c2]))))
  (>!! c1 "foo")
  (assert (= (<!! r) ["foo" c1]))
  (>!! c2 "bar")
  (assert (= (<!! r) ["bar" c2])))
```

`alt!`は複数のchannelの操作から一つ選択する.

```clojure
(require '[clojure.core.async :refer (chan >! <!! >!! alt! go)])

(let [c1 (chan)
      c2 (chan)
      c3 (chan)
      r (chan)]
  (go (while true
        (>! r (alt! [c1 c2] ([v c] [v c])
                    c3 ([v] v)))))
  (>!! c1 "foo")
  (assert (= (<!! r) ["foo" c1]))
  (>!! c2 "bar")
  (assert (= (<!! r) ["bar" c2]))
  (>!! c3 "baz")
  (assert (= (<!! r) "baz")))
```

これらの関数は`timeout`と組み合わせると有用である.

```clojure
(require '[clojure.java.io :refer (as-url)])
(require '[clojure.core.async :refer (chan timeout >! >!! alt! go)])

(let [c (chan)]
  (go (alt! c ([v] (spit "clojuredocs.html" v))
            (timeout 10000) ([] (prn "timeout"))))
  (go (>! c (slurp (as-url "http://clojuredocs.org")))))
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
