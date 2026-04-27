(in-package #:mylib/tests/syntax)

;; NOTE: To run this test file, execute `(asdf:test-system :mylib)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
