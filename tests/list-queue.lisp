(in-package #:mylib/tests/list-queue)

(deftest list-queue-basic
  (let ((q (make-list-queue)))
    (ok (list-queue-empty-p q))
    (ok (equal nil (list-queue-raw q)))
    (list-queue-enqueue q 10)
    (ok (not (list-queue-empty-p q)))
    (ok (= 10 (list-queue-peek q)))
    (ok (= 10 (list-queue-dequeue q)))
    (ok (list-queue-empty-p q))
    (ok (equal nil (list-queue-raw q)))))

(deftest list-queue-fifo-order
  (let ((q (make-list-queue)))
    (dolist (x '(1 2 3 4 5))
      (list-queue-enqueue q x))
    (ok (equal '(1 2 3 4 5) (list-queue-raw q)))
    (ok (equal '(1 2 3 4 5)
               (loop repeat 5 collect (list-queue-dequeue q))))
    (ok (list-queue-empty-p q))))

(deftest list-queue-reuse-after-empty
  (let ((q (make-list-queue)))
    (list-queue-enqueue q :a)
    (list-queue-enqueue q :b)
    (ok (eq :a (list-queue-dequeue q)))
    (ok (eq :b (list-queue-dequeue q)))
    (ok (list-queue-empty-p q))
    (list-queue-enqueue q :c)
    (ok (eq :c (list-queue-peek q)))
    (ok (eq :c (list-queue-dequeue q)))
    (ok (list-queue-empty-p q))))

(deftest list-queue-empty-errors
  (let ((q (make-list-queue)))
    (ok (handler-case
            (progn (list-queue-peek q) nil)
          (error () t)))
    (ok (handler-case
            (progn (list-queue-dequeue q) nil)
          (error () t)))))
