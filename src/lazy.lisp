(in-package #:mylib.lazy)

(defstruct promise (value nil) thunk)

(defmacro delay (expr) `(make-promise :thunk (lambda () ,expr)))

(defun force (ps)
  (when (promise-thunk ps)
    (setf (promise-value ps) (funcall (promise-thunk ps))
          (promise-thunk ps) nil))
  (promise-value ps))

(defmacro lcons (car cdr)
  "遅延された cons cell を生成する。"
  `(delay (cons ,car ,cdr)))

(defun lcar (cell)
  "遅延された cons cell の car を返す。"
  (car (force cell)))

(defun lcdr (cell)
  "遅延された cons cell の cdr を返す。"
  (cdr (force cell)))

(defmacro lazy-let (binds &body body)
  "遅延評価され、相互再帰できる binding を定義する。
実装上、binding 名は symbol macro として定義される。"
  (let ((promise-vars (mapcar (lambda (bind)
                                (declare (ignore bind))
                                (gensym "PROMISE"))
                              binds)))
    `(let ,(mapcar (lambda (promise-var)
                     `(,promise-var nil))
                   promise-vars)
       (symbol-macrolet
           ,(mapcar (lambda (bind promise-var)
                      `(,(car bind) (force ,promise-var)))
                    binds
                    promise-vars)
         (setf ,@(mapcan (lambda (bind promise-var)
                           (list promise-var `(delay ,(cadr bind))))
                         binds
                         promise-vars))
         ,@body))))

(defmacro define-lazy-constant (name value &optional documentation)
  "遅延評価されるグローバルな定数を定義する。
NAME は参照時に一度だけ VALUE を評価した結果を表す。
実装上、NAME は symbol macro として定義される。"
  (let ((storage (gensym "LAZY-CONSTANT")))
    `(progn
       (defparameter ,storage
         (delay
           (symbol-macrolet ((,name (force ,storage)))
             ,value))
         ,documentation)
       (define-symbol-macro ,name (force ,storage))
       ',name)))
