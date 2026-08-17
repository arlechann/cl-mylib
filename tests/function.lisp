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

(deftest pa-fills-positional-placeholders
  (ok (equal '(b a)
             (funcall (pa #'list :$1 :$0) 'a 'b)))
  (ok (equal '(head x y z)
             (funcall (pa #'list 'head :$0 :$@) 'x 'y 'z)))
  (ok (equal '(left middle right)
             (funcall (pa #'list :$0 'middle :$1) 'left 'right))))

(deftest pa-evaluates-fixed-forms-at-call-time
  (let* ((n 0)
         (f (pa #'list (incf n) :$0)))
    (ok (equal '(1 a) (funcall f 'a)))
    (ok (equal '(2 b) (funcall f 'b)))))

(deftest pa*-captures-fixed-forms-at-creation-time
  (let ((n 0))
    (let ((f (pa* #'list (incf n) :$0)))
      (ok (equal '(1 a) (funcall f 'a)))
      (ok (equal '(1 b) (funcall f 'b)))
      (ok (= 1 n)))))
