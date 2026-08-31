(in-package #:mylib/tests/function)

(deftest compose-combines-functions
  (ok (= 3
         (funcall (compose #'1+ #'abs) -2)))
  (ok (equal '(3 4)
             (multiple-value-list
              (funcall (compose #'values
                                (lambda (x)
                                  (values x (1+ x))))
                       3))))
  (ok (equal '(1 2 3)
             (multiple-value-list
              (funcall (compose) 1 2 3)))))

(deftest conjoin-short-circuits-and-returns-boolean
  (let ((count 0))
    (ok (funcall (conjoin #'integerp #'plusp) 10))
    (ok (not (funcall (conjoin #'integerp #'plusp) -1)))
    (ok (not (funcall (conjoin (lambda (x)
                                 (declare (ignore x))
                                 (incf count)
                                 nil)
                               (lambda (x)
                                 (declare (ignore x))
                                 (incf count)
                                 t))
                       :ignored)))
    (ok (= 1 count))))

(deftest disjoin-short-circuits-and-returns-boolean
  (let ((count 0))
    (ok (funcall (disjoin #'integerp #'stringp) 10))
    (ok (funcall (disjoin #'integerp #'stringp) "x"))
    (ok (not (funcall (disjoin #'integerp #'stringp) 3.5)))
    (ok (funcall (disjoin (lambda (x)
                            (declare (ignore x))
                            (incf count)
                            t)
                          (lambda (x)
                            (declare (ignore x))
                            (incf count)
                            nil))
                 :ignored))
    (ok (= 1 count))))

(deftest flip-reverses-all-arguments
  (ok (null (do-nothing 1 2 3)))
  (ok (= 7
         (funcall (flip #'-) 3 10)))
  (ok (equal '(3 2 1)
             (funcall (flip #'list) 1 2 3)))
  (ok (equal '(c b a)
             (funcall (flip #'list) 'a 'b 'c))))

(deftest pa/pa*-function-helpers
  (let ((f (pa #'list :$0 :$@)))
    (ok (equal '(1 2 3) (funcall f 1 2 3))))
  (let ((f (pa #'list :$0 :$*)))
    (ok (equal '(1 (2 3)) (funcall f 1 2 3))))
  (ok (equal '(2 4 6)
             (funcall (pa #'mapcar
                          #'(lambda (x) (* (1+ x) 2))
                          :$0)
                      '(0 1 2))))
  (let ((x 10))
    (let ((f (pa* #'+ x :$0)))
      (setf x 100)
      (ok (= 13 (funcall f 3)))))
  (let ((x 10))
    (let ((f (pa* #'list x :$*)))
      (setf x 100)
      (ok (equal '(10 (2 3)) (funcall f 2 3)))))
  (let ((x 10))
    (let ((f (pa* #'mapcar
                  #'(lambda (y) (+ x y))
                  :$0)))
      (setf x 100)
      (ok (equal '(101 102 103)
                 (funcall f '(1 2 3)))))))

(deftest fn/fn*-function-helpers
  (ok (equal '(2 4 6)
             (mapcar (fn (* (1+ :$0) 2))
                     '(0 1 2))))
  (ok (= 8
         (funcall (fn (expt :$0 :$1))
                  2 3)))
  (ok (= 8
         (funcall (fn (funcall #'(lambda (x y) (expt x y))
                               :$0
                               :$1))
                  2 3)))
  (ok (equal '(1 (2 3 4))
             (funcall (fn (list :$0 :$*))
                      1 2 3 4)))
  (let ((x 10))
    (let ((f (fn* (+ x :$0))))
      (setf x 100)
      (ok (= 13 (funcall f 3)))))
  (let ((x 10))
    (let ((f (fn* (funcall #'(lambda (y) (+ x y :$0))
                           1))))
      (setf x 100)
      (ok (= 14 (funcall f 3)))))
  (let ((x 10))
    (let ((f (fn* (list x :$*))))
      (setf x 100)
      (ok (equal '(10 (2 3)) (funcall f 2 3))))))