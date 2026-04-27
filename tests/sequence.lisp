(in-package #:mylib/tests/sequence)

(deftest sequence-mapping
  (ok (= 6 (sum '(1 2 3))))
  (ok (equal '(0 2 4)
             (map-with-index 'list
                             (lambda (i x)
                               (+ i x))
                             '(0 1 2))))
  (let ((target (vector 0 0 0)))
    (map-into-with-index target
                         (lambda (i x)
                           (+ i x))
                         #(1 1 1))
    (ok (equalp #(1 2 3) target)))
  (let ((target (vector 1 2 3)))
    (nmap (lambda (x) (* x 2)) target)
    (ok (equalp #(2 4 6) target)))
  (let ((target (vector 1 1 1)))
    (nmap-with-index (lambda (i x) (+ i x)) target)
    (ok (equalp #(1 2 3) target))))

(deftest indexed-reduce-and-find
  (ok (= 9
         (reduce-with-index (lambda (i acc x)
                              (+ acc x i))
                            #(1 2 3)
                            :initial-value 0)))
  (ok (equal '(2 . 3)
             (find-with-index (lambda (i x)
                                (and (= i 2) (= x 3)))
                              #(1 2 3 4))))
  (ok (null (find-with-index (lambda (i x)
                               (declare (ignore i x))
                               nil)
                             #(1 2 3)))))

(deftest arg-operations
  (ok (= 1 (argmax #(1 5 3 5))))
  (ok (= 3 (argmax #(1 5 3 5) :from-end t)))
  (ok (= 2 (argmin #(3 2 1 1))))
  (ok (= 3 (argmin #(3 2 1 1) :from-end t)))
  (ok (= 1 (argopt #'>
                   #((1 . 10) (2 . 20) (3 . 15))
                   :key #'cdr))))
