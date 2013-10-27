# Compatible Clojure

ClojureとClojureScriptの両方で動くコードの書き方

# crossovers

もっとも重要なものとして,lein-cljsbuildの機能にcrossoversがある.

```clojure
(defproject example "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild {:builds [{}]
              :crossovers [example.core]})
```

これは,指定したnamespaceをClojureScriptのコンパイル時にまるごとコピーする仕組みである.

# CLJSBUILD-REMOVE

lein-cljsbuildによるプリプロセッサがいくつか存在する.

その一つとして,コンパイル時に取り除かれるコメントが存在する.

```clojure
(ns example
  (:require;*CLJSBUILD-REMOVE*;-macros
    [example.macros :as macros]))
```

上記のコードは,cljsとしてコピーされた時,以下のようなコードになる.

```clojure
(ns example
  (:require-macros
    [example.macros :as macros]))
```

これで,ClojureとClojureScriptの両方でマクロを使うことが出来る.

また,以下のような使い方もある.

```clojure
(extend-protocol Foo
  ;*CLJSBUILD-REMOVE*;cljs.core.PersistentVector #_
  clojure.lang.IPersistentVector
  (bar [this] baz))
```

これらのテクニックを用いれば,ClojureとClojureScript間の名前の違いによるエラーを回避できる.

しかし,この機能はあくまでlein-cljsbuildが提供するものであり,ClojureやClojureScriptが提供するものではないということに留意したい.

# Macro

コンパイル単位を分けるため,別ファイルに記述する必要がある.

上記の方法で,同一のコードに`require`を記述することが可能である.

# Types

clojure.langに定義されるクラスは大抵の場合cljs.coreに定義される.

また,ClojureScriptでは,java.lang.Stringとjava.lang.Objectを拡張したいときにjs/Stringとjs/Objectを拡張するのではなく,stringとobjectを拡張する.

```clojure
(extend-protocol Foo
  ;*CLJSBUILD-REMOVE*;string #_
  java.lang.String
  (bar [this] baz)
  ;*CLJSBUILD-REMOVE*;object #_
  java.lang.Object
  (bar [this] qux))
```
