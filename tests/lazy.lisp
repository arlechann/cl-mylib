(in-package #:mylib/tests/lazy)

(deftest delay-and-force
  (let* ((cnt 0)
         (p (delay (progn (incf cnt) 42))))
    (ok (= 42 (force p)))
    (ok (= 42 (force p)))
    (ok (= 1 cnt))))
