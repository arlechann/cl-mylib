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

(deftest sequence-and-array-iteration-macros
  (let ((sum 0))
    (ok (= 6
           (do-array (x #(1 2 3) sum)
             (incf sum x)))))
  (let ((sum 0))
    (ok (= 10
           (do-array (x #2A((1 2) (3 4)) sum)
             (incf sum x)))))
  (let ((sum 0))
    (ok (= 21
           (do-array* ((x y) (#(1 2 3) #(4 5 6 7)) sum)
             (incf sum (+ x y))))))
  (let ((sum 0))
    (ok (= 110
           (do-array* ((x y) (#2A((1 2) (3 4)) #2A((10 20) (30 40) (50 60))) sum)
             (incf sum (+ x y))))))
  (let ((sum 0))
    (ok (= 6
           (do-seq (x '(1 2 3) sum)
             (incf sum x)))))
  (let ((sum 0))
    (ok (= 21
           (do-seq* ((x y) ((list 1 2 3) (list 4 5 6)) sum)
             (incf sum (+ x y)))))))

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
  (ok (= 3 (and-let* ((x 1) (y (+ x 1))) (+ x y))))
  (ok (null (and-let* ((x 1) (y nil)) (+ x y))))
  (ok (= 2 (and-let* ((x 1) (y (+ x 1))))))
  (let ((count 0))
    (ok (null (and-let* ((x nil)
                         (y (incf count)))
                (list x y))))
    (ok (= 0 count)))
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

(deftest debug-print-prints-expression-and-value
  (let ((output (with-output-to-string (*error-output*)
                  (ok (= 3
                         (debug-print (+ 1 2)))))))
    (ok (search "DEBUG: (+ 1 2) => 3" output))))

(deftest debug-print*-prints-selected-bindings-and-value
  (let ((x 10)
        (y 20))
    (let ((output (with-output-to-string (*error-output*)
                    (ok (= 30
                           (debug-print* (x y) (+ x y)))))))
      (ok (search "DEBUG*: X=10 Y=20 (+ X Y) => 30" output)))))
