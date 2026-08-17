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

(deftest sequence-construction-and-slicing
  (ok (equal '((a . 2) (b . 3))
             (run-length-encode '(a a b b b))))
  (let ((v (vector* 1 2 3)))
    (ok (equal '(1 2 3) (coerce v 'list)))
    (ok (array-has-fill-pointer-p v))
    (ok (adjustable-array-p v)))
  (let* ((v #(1 2 3 4))
         (sub (displaced-subvec v :start 1 :end 3)))
    (ok (equal '(2 3) (coerce sub 'list)))
    (setf (aref sub 0) 20)
    (ok (= 20 (aref v 1)))))

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

(deftest window-map-basic
  (ok (equal '(3 5 7)
             (window-map 'list 2 #'+ '(1 2 3 4))))
  (ok (equal '(6 24)
             (window-map 'list 3 #'* '(1 2 3 4))))
  (ok (equal '(1 2 3)
             (window-map 'list 1 #'identity '(1 2 3)))))

(deftest window-map-boundary
  (ok (handler-case (progn (window-map 'list 5 #'+ '(1 2 3))
                           nil)
        (error () t)))
  (ok (handler-case (progn (window-map 'list 4 #'+ '(1 2 3))
                           nil)
        (error () t)))
  (ok (handler-case (progn (window-map 'list 2 #'+ nil)
                           nil)
        (error () t)))
  (ok (equal '(6)
             (window-map 'list 3 #'+ '(1 2 3)))))

(deftest window-nmap-vector
  (let ((v (vector 1 2 3 4)))
    (ok (equal '(3 5 7)
               (coerce (window-nmap 2 #'+ v) 'list)))
    (ok (equal '(3 5 7 7) (coerce v 'list))))
  (let ((v (vector 1 2 3)))
    (ok (handler-case (progn (window-nmap 4 #'+ v)
                             nil)
          (error () t)))))

(deftest window-nmap-list
  (let ((lst (list 1 2 3 4)))
    (ok (equal '(3 5 7)
               (window-nmap 2 #'+ lst)))
    (ok (equal '(nil 3 5 7) lst)))
  (let ((lst (list 1 2 3)))
    (ok (handler-case (progn (window-nmap 4 #'+ lst)
                             nil)
          (error () t)))))
