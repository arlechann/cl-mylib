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
- `mylib.function`
  - 関数合成、述語合成、部分適用マクロ
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

## インターフェース

### `mylib.syntax`

Macro: **eval-always** `&body body`

Macro: **with-gensyms** `symbols &body body`

Macro: **nlet** `name binds &body body`

Macro: **while** `test &body body`

Macro: **until** `test &body body`

Macro: **aif** `test then &optional else`

Macro: **alambda** `params &body body`

Macro: **aprog1** `result &body body`

Macro: **aand** `&rest args`

Macro: **acond** `&rest clauses`

Macro: **if-let** `binds then &optional else`

Macro: **if-let\*** `binds then &optional else`

Macro: **and-let\*** `binds &body body`

Macro: **when-let** `binds &body body`

Macro: **when-let\*** `binds &body body`

Macro: **debug-print** `expr`

Macro: **debug-print\*** `(vars) expr`

### `mylib.function`

Function: **flip** function

Function: **compose** `&rest functions`

Function: **conjoin** `&rest predicates`

Function: **disjoin** `&rest predicates`

Macro: **pa** `function &rest forms`

Macro: **pa\*** `function &rest forms`

`pa` / `pa*` では `:$0`, `:$1`, ... を位置プレースホルダとして使います。`:$@` は残りの引数をその位置に展開します。`pa*` はプレースホルダでない式を関数生成時に評価します。

### `mylib.number`

Variable: **\*eps\***

Function: **square** x

Function: **clamp** `x low high`

Function: **maxp** `x &rest args`

Function: **minp** `x &rest args`

Macro: **maxf** `place &rest args`

Macro: **minf** `place &rest args`

Function: **lerp** `a b ratio`

Function: **approx=** `x y &key (eps *eps*)`

Function: **approx-zero-p** `x &key (eps *eps*)`

Function: **approx<=** `x y &key (eps *eps*)`

Function: **approx>=** `x y &key (eps *eps*)`

### `mylib.sequence`

Function: **sum** sequence

Macro: **sortf** `place compare &rest args`

Function: **map-with-index** `result-type fn sequence &rest more-sequences`

Function: **map-into-with-index** `result-sequence fn &rest sequences`

Function: **nmap** `fn sequence &rest more-sequences`

Function: **nmap-with-index** `fn sequence &rest more-sequences`

Function: **reduce-with-index** `function sequence &key key from-end (start 0) end initial-value`

Function: **find-with-index** `predicate sequence &key from-end start end key`

Function: **argopt** `predicate sequence &key key from-end start end`

Function: **argmax** `sequence &key key from-end start end`

Function: **argmin** `sequence &key key from-end start end`

### `mylib.list`

Function: **ensure-car** list

Function: **ensure-list** obj

Function: **xcons** `cdr car`

Function: **tconc** `pointer obj`

Function: **lconc** `pointer list`

Function: **last1** list

Function: **length=** `list n`

Function: **length<** `list n`

Function: **length>** `list n`

Function: **length<=** `list n`

Function: **length>=** `list n`

Function: **take** `list n`

Function: **drop** `list n`

Function: **filter-map** `fn list &rest more-list`

Function: **iota** `count &key (start 0) (step 1)`

Function: **unique** `list &key (test #'eql)`

Function: **flatten** list

Function: **join** `list separator`

Macro: **with-collector** `(&rest collectors) &body body`

### `mylib.algorithm`

Function: **meguru-method** `ok ng predicate`

Function: **binary-search** `ok ng predicate &key (eps mylib.number:*eps*) (max-iteration 300)`

Function: **lower-bound** `vector element &key (start 0) end`

Function: **upper-bound** `vector element &key (start 0) end`

### `mylib.amb`

Variable: **\*failed\***

Function: **amb-reset**

Macro: **amb** `&rest options`

Macro: **amb-bind** `var options &body body`
