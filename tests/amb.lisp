(in-package #:mylib/tests/amb)

(deftest amb-basic-choice
  (amb-reset)
  (let ((*failed* :failed))
    (ok (eql 'a (amb 'a 'b 'c)))
    (ok (eql 'b (mylib.amb::fail)))
    (ok (eql 'c (mylib.amb::fail)))
    (ok (eql :failed (mylib.amb::fail)))))

(deftest amb-empty-choice
  (amb-reset)
  (let ((*failed* :failed))
    (ok (eql :failed (amb)))))

(deftest amb-bind-iterates-options
  (amb-reset)
  (let ((*failed* :failed))
    (ok (= 2 (amb-bind x '(1 2 3)
               (if (evenp x)
                   x
                   (mylib.amb::fail)))))
    (ok (eql :failed (mylib.amb::fail)))))
