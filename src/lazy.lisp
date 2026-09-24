(in-package #:mylib.lazy)

(defstruct promise (value nil) thunk)

(defmacro delay (expr)
  "EXPR の評価を遅延し、最初の FORCE 時に一度だけ評価する promise を生成する。"
  `(make-promise :thunk (lambda () ,expr)))

(defun force (value)
  "PROMISE なら一度だけ評価し、それ以外の VALUE はそのまま返す。"
  (if (promise-p value)
      (progn
        (when (promise-thunk value)
          (setf (promise-value value) (funcall (promise-thunk value))
                (promise-thunk value) nil))
        (promise-value value))
      value))

(define-compiler-macro force (&whole form value)
  (if (constantp value)
      value
      form))

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
