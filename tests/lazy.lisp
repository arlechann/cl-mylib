(in-package #:mylib/tests/lazy)

(defparameter *lazy-constant-evaluation-count* 0)

(defun fresh-lazy-constant-name ()
  (intern (symbol-name (gensym "TEST-LAZY-CONSTANT-"))
          (find-package '#:mylib/tests/lazy)))

(deftest delay-and-force
  (let* ((cnt 0)
         (p (delay (progn (incf cnt) 42))))
    (ok (= 42 (force p)))
    (ok (= 42 (force p)))
    (ok (= 1 cnt))))

(deftest define-lazy-constant-delays-and-memoizes
  (let ((name (fresh-lazy-constant-name)))
    (setf *lazy-constant-evaluation-count* 0)
    (eval `(define-lazy-constant ,name
             (progn
               (incf *lazy-constant-evaluation-count*)
               42)))
    (ok (= 0 *lazy-constant-evaluation-count*))
    (ok (= 42 (eval name)))
    (ok (= 1 *lazy-constant-evaluation-count*))
    (ok (= 42 (eval name)))
    (ok (= 1 *lazy-constant-evaluation-count*))
    (ok (not (boundp name)))))

(deftest define-lazy-constant-supports-recursive-values
  (let ((name (fresh-lazy-constant-name)))
    (eval `(define-lazy-constant ,name
             (lcons 1 ,name)))
    (ok (equal '(1 1 1)
               (eval `(list (lcar ,name)
                            (lcar (lcdr ,name))
                            (lcar (lcdr (lcdr ,name)))))))))

(deftest lazy-list-low-level-api
  (let* ((eval-count 0)
         (cell (lcons (progn (incf eval-count) :head)
                      (progn (incf eval-count) :tail))))
    (ok (= 0 eval-count))
    (ok (eq :head (lcar cell)))
    (ok (= 2 eval-count))
    (ok (eq :head (lcar cell)))
    (ok (= 2 eval-count))
    (ok (eq :tail (lcdr cell)))))

(deftest lazy-let-delays-and-memoizes-bindings
  (let ((cnt 0))
    (ok (= 42
           (lazy-let ((answer (progn (incf cnt) 42)))
             (ok (= 0 cnt))
             answer)))
    (ok (= 1 cnt))))

(deftest lazy-let-supports-recursive-bindings
  (ok (= 55
         (lazy-let ((fib (lambda (n)
                           (if (< n 2)
                               n
                               (+ (funcall fib (- n 1))
                                  (funcall fib (- n 2)))))))
             (funcall fib 10))))
  (ok (equal '(1 1 1)
             (lazy-let ((ones (lcons 1 ones)))
               (list (lcar ones)
                     (lcar (lcdr ones))
                     (lcar (lcdr (lcdr ones))))))))
