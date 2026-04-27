(in-package #:mylib/tests/syntax)

;; NOTE: To run this test file, execute `(asdf:test-system :mylib)' in your Lisp.

(deftest with-gensyms-generates-distinct-symbols
  (multiple-value-bind (a b)
      (with-gensyms (a b)
        (values a b))
    (ok (symbolp a))
    (ok (symbolp b))
    (ng (eq a b))))

(deftest nlet-works-like-named-loop
  (ok (= 15
         (nlet sum ((i 1)
                    (acc 0))
           (if (> i 5)
               acc
               (sum (1+ i) (+ acc i)))))))

(deftest while-and-until-loop
  (let ((i 0)
        (acc nil))
    (while (< i 3)
      (push i acc)
      (incf i))
    (ok (equal '(2 1 0) acc)))
  (let ((i 0))
    (until (= i 4)
      (incf i))
    (ok (= 4 i))))

(deftest anaphoric-macros
  (ok (= 6 (aif (+ 1 2) (+ it 3) 0)))
  (ok (null (aif nil (+ it 3) nil)))
  (ok (= 10 (aprog1 (+ 4 6) (setf it 1))))
  (ok (= 6 (funcall (alambda (n)
                      (if (zerop n)
                          0
                          (+ n (self (1- n)))))
                    3)))
  (ok (= 3 (aand 1 2 3)))
  (ok (null (aand 1 nil 3)))
  (ok (= 20
         (acond ((find 2 '(1 2 3)) (* it 10))
                ((find 3 '(1 2 3)) (* it 100))))))

(deftest let-like-macros
  (ok (= 3 (if-let ((x 1) (y 2)) (+ x y) 0)))
  (ok (= 0 (if-let ((x 1) (y nil)) (+ x y) 0)))
  (ok (= 3 (if-let* ((x 1) (y (+ x 1))) (+ x y) 0)))
  (ok (= 3
         (let ((sum 0))
           (when-let ((x 1) (y 2))
             (setf sum (+ x y)))
           sum)))
  (ok (= 3
         (let ((sum 0))
           (when-let* ((x 1) (y (+ x 1)))
             (setf sum (+ x y)))
           sum))))
