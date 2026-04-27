(in-package #:mylib/tests/algorithm)

(deftest meguru-method-finds-last-true-index
  (ok (= 4
         (meguru-method 0 10
                        (lambda (x)
                          (<= (* x x) 20))))))

(deftest lower-and-upper-bound
  (let ((vec #(1 2 2 4 7)))
    (ok (= 1 (lower-bound vec 2)))
    (ok (= 3 (upper-bound vec 2)))
    (ok (= 0 (lower-bound vec 0)))
    (ok (= 5 (lower-bound vec 9)))
    (ok (= 4 (lower-bound vec 5)))
    (ok (= 5 (upper-bound vec 7)))
    (ok (= 2 (lower-bound vec 2 :start 2)))
    (ok (= 4 (lower-bound vec 5 :start 1 :end 4)))))
