# mylib

Common Lisp 用の個人ユーティリティ集です。

## インストール

ASDF から `mylib` system をロードしてください。

```lisp
(asdf:load-system :mylib)
```

テストを実行する場合は `mylib/tests` を利用します。

```lisp
(asdf:test-system :mylib)
```

## 使い方

用途ごとに package を分けています。

- `mylib.syntax`
  - 制御構文や補助マクロ
- `mylib.number`
  - 数値処理の小関数・マクロ
- `mylib.sequence`
  - sequence 操作の補助
- `mylib.list`
  - list 構築・加工の補助
- `mylib.algorithm`
  - 二分探索系のアルゴリズム
- `mylib.amb`
  - `amb` による非決定的計算

たとえば `mylib.list` を使う場合は次のように読み込みます。

```lisp
(use-package :mylib.list)
```
