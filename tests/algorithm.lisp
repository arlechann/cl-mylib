(in-package #:mylib/tests/algorithm)

(deftest meguru-method-finds-last-true-index
  (ok (= 4
         (meguru-method 0 10
                        (lambda (x)
                          (<= (* x x) 20))))))

(deftest binary-search-finds-boundary
  (let ((result (binary-search 0d0 2d0
                               (lambda (x)
                                 (<= (* x x) 2d0))
                               :eps 1d-10)))
    (ok (mylib.number:approx= result
                              (sqrt 2d0)
                              :eps 1d-10))
    (ok (<= (* result result) 2d0))
    (ok (> (* (+ result 1d-10) (+ result 1d-10)) 2d0))))

(deftest binary-search-respects-max-iteration
  (ok (= 1
         (binary-search 0 2
                        (lambda (x)
                          (<= x 1))
                        :eps 0
                        :max-iteration 1))))

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
