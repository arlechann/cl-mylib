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

通常は `mylib` package を使えば `mylib.*` の公開シンボルがまとめて利用できます。

```lisp
(use-package :mylib)
```

用途ごとに個別 package を使うこともできます。

- `mylib.syntax`
  - 制御構文や補助マクロ
- `mylib.function`
  - 関数合成、述語合成、部分適用マクロ
- `mylib.number`
  - 数値処理の小関数・マクロ
- `mylib.sequence`
  - sequence 操作の補助
  - スライディングウィンドウ処理
- `mylib.list`
  - list 構築・加工の補助
- `mylib.list-queue`
  - リストベースのキュー
- `mylib.string`
  - 文字列処理の補助
- `mylib.lazy`
  - 遅延評価
- `mylib.algorithm`
  - 二分探索系のアルゴリズム
- `mylib.amb`
  - `amb` による非決定的計算

たとえば `mylib.list` だけを使う場合は次のように読み込みます。

```lisp
(use-package :mylib.list)
```

## インターフェース

### `mylib.syntax`

Macro: **eval-always** `&body body`

Macro: **with-gensyms** `symbols &body body`

Macro: **do-array** `(var array &optional result) &body body`

Macro: **do-array\*** `((vars) (arrays) &optional result) &body body`

Macro: **do-seq** `(var sequence &optional result) &body body`

Macro: **do-seq\*** `((vars) (sequences) &optional result) &body body`

Macro: **named-let** `name binds &body body`

Macro: **nlet** `name binds &body body`

`named-let` は名前付き再帰をローカルなループへ展開するマクロです。

`nlet` は `named-let` の alias です。

Macro: **block-lambda** `params &body body`

Macro: **named-lambda** `name params &body body`

Macro: **nlambda** `name params &body body`

`named-lambda` は `labels` を使って名前付きの関数オブジェクトを生成します。

`nlambda` は `named-lambda` の alias です。

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

Function: **do-nothing** `&rest args`

Function: **flip** `function`

Function: **compose** `&rest functions`

Function: **conjoin** `&rest predicates`

Function: **disjoin** `&rest predicates`

Macro: **pa** `function &rest forms`

Macro: **pa\*** `function &rest forms`

`pa` / `pa*` は、プレースホルダを使って関数に部分適用するマクロです。`:$0`, `:$1`, ... を位置プレースホルダとして使います。`:$*` は残りの引数全体を 1 つのリストとして渡し、`:$@` は残りの引数をその位置に展開します。`pa*` はプレースホルダでない式を関数生成時に評価します。

```lisp
(funcall (pa #'list :$1 :$0) 'a 'b)
;; => (B A)

(funcall (pa #'list :$0 :$*) 1 2 3)
;; => (1 (2 3))

(funcall (pa #'list :$0 :$@) 1 2 3)
;; => (1 2 3)

(let ((x 10))
  (let ((f (pa* #'+ x :$0)))
    (setf x 100)
    (funcall f 3)))
;; => 13
```

Macro: **fn** `&body body`

Macro: **fn\*** `&body body`

`fn` / `fn*` は、暗黙的引数を扱う関数構築マクロです。`:$0`, `:$1`, ... を位置プレースホルダとして使います。`:$*` は残りの引数全体を 1 つのリストとして参照します。`fn*` はプレースホルダでない式を関数生成時に評価します。

```lisp
(mapcar (fn (* (1+ :$0) 2)) '(0 1 2))
;; => (2 4 6)

(funcall (fn (list :$0 :$*)) 1 2 3 4)
;; => (1 (2 3 4))

(let ((x 10))
  (let ((f (fn* (+ x :$0))))
    (setf x 100)
    (funcall f 3)))
;; => 13
```

### `mylib.number`

Variable: **\*eps\*** `1d-12`

Function: **square** `x`

Function: **cube** `x`

Function: **pow** `base power &key (op #'*) (identity 1)`

Function: **diff** `a b`

Function: **next-pow2** `n`

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

Function: **sum** `sequence`

Macro: **sortf** `place compare &rest args`

Macro: **reversef** `place`

Macro: **nreversef** `place`

Function: **map-with-index** `result-type fn sequence &rest more-sequences`

Function: **map-into-with-index** `result-sequence fn &rest sequences`

Function: **nmap** `fn sequence &rest more-sequences`

Function: **nmap-with-index** `fn sequence &rest more-sequences`

Function: **reduce-with-index** `function sequence &key key from-end (start 0) end initial-value`

Function: **find-with-index** `predicate sequence &key from-end start end key`

Function: **argopt** `predicate sequence &key key from-end start end`

Function: **argmax** `sequence &key key from-end start end`

Function: **argmin** `sequence &key key from-end start end`

Function: **window-map** `result-type window-size fn sequence`

Function: **window-nmap** `window-size fn sequence`

Function: **run-length-encode** `sequence &key (test #'eql)`

Function: **vector\*** `&rest contents`

Function: **displaced-subvec** `vector &key (start 0) end`

### `mylib.list`

Function: **ensure-car** `list`

Function: **ensure-list** `obj`

Function: **xcons** `cdr car`

Function: **mapc-with-index** `fn list &rest more-lists`

Function: **mapcar-with-index** `fn list &rest more-lists`

Function: **mapcan-with-index** `fn list &rest more-lists`

Function: **mapl-with-index** `fn list &rest more-lists`

Function: **maplist-with-index** `fn list &rest more-lists`

Function: **mapcon-with-index** `fn list &rest more-lists`

Function: **tconc** `pointer obj`

Function: **lconc** `pointer list`

Function: **singlep** `list`

Function: **last1** `list`

Function: **length=** `list n`

Function: **length<** `list n`

Function: **length>** `list n`

Function: **length<=** `list n`

Function: **length>=** `list n`

Function: **take** `list n`

Function: **drop** `list n`

Function: **filter-map** `fn list &rest more-list`

Function: **iota** `count &key (start 0) (step 1)`

Function: **longerp** `lst1 lst2`

Function: **longer** `lst1 lst2`

Function: **unfold** `predicate fn next-generator seed &optional tail`

Function: **unique** `list &key (test #'eql)`

Function: **chunks** `lst size &key (fractionp t)`

Function: **flatten** `list`

Function: **join** `list separator`

Macro: **with-collector** `(&rest collectors) &body body`

### `mylib.algorithm`

Function: **meguru-method** `ok ng predicate`

Function: **binary-search** `ok ng predicate &key (eps mylib.number:*eps*) (max-iteration 300)`

Function: **lower-bound** `vector element &key (start 0) end`

Function: **upper-bound** `vector element &key (start 0) end`

### `mylib.string`

Function: **strjoin** `strings &key (spacer (string #\Newline))`

Function: **trim-whitespace** `string`

### `mylib.lazy`

Macro: **delay** `expr`

Function: **force** `promise`

### `mylib.list-queue`

Function: **make-list-queue**

Function: **list-queue-empty-p** `queue`

Function: **list-queue-peek** `queue`

Function: **list-queue-raw** `queue`

Function: **list-queue-enqueue** `queue value`

Function: **list-queue-dequeue** `queue`

### `mylib.amb`

Variable: **\*failed\*** `nil`

Function: **amb-reset**

Macro: **amb** `&rest options`

Macro: **amb-bind** `var options &body body`
