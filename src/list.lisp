(in-package #:mylib.list)

(declaim (ftype (function (t) t) ensure-car))
(defun ensure-car (list)
  (if (consp list)
      (car list)
      list))

(declaim (ftype (function (t) list) ensure-list))
(defun ensure-list (obj)
  (if (or (null obj) (consp obj))
      obj
      (list obj)))

(declaim (ftype (function (t t) cons) xcons))
(defun xcons (cdr car)
  (cons car cdr))

(declaim (ftype (function (list t) cons) tconc))
(defun tconc (pointer obj)
  (when (null pointer)
    (setf pointer (cons nil nil)))
  (let ((cell (cons obj nil)))
    (if (null (car pointer))
        (setf (car pointer) cell
              (cdr pointer) cell)
        (setf (cdr (cdr pointer)) cell
              (cdr pointer) cell))
    pointer))

(declaim (ftype (function (list list) cons) lconc))
(defun lconc (pointer list)
  (when (null pointer)
    (setf pointer (cons nil nil)))
  (when (null list)
    (return-from lconc pointer))
  (if (null (car pointer))
      (setf (car pointer) list
            (cdr pointer) (last list))
      (setf (car pointer) (nconc (car pointer) list)
            (cdr pointer) (last list)))
  pointer)

(declaim (ftype (function (list) t) last1))
(defun last1 (list)
  (car (last list)))

(declaim (ftype (function (list fixnum) boolean) length=))
(defun length= (list n)
  (loop when (<= n 0)
          return (null list)
        when (null list)
          return nil
        do (setf list (cdr list)
                 n (1- n))))

(declaim (ftype (function (list fixnum) boolean) length<))
(defun length< (list n)
  (loop when (<= n 0)
          return nil
        when (null list)
          return t
        do (setf list (cdr list)
                 n (1- n))))

(declaim (ftype (function (list fixnum) boolean) length<=))
(defun length<= (list n)
  (loop when (<= n 0)
          return (null list)
        when (null list)
          return t
        do (setf list (cdr list)
                 n (1- n))))

(declaim (ftype (function (list fixnum) boolean) length>))
(defun length> (list n)
  (not (length<= list n)))

(declaim (ftype (function (list fixnum) boolean) length>=))
(defun length>= (list n)
  (not (length< list n)))

(declaim (ftype (function (list fixnum) list) take))
(defun take (list n)
  (do ((list list (cdr list))
       (n n (1- n))
       (acc nil))
      ((or (<= n 0) (null list)) (nreverse acc))
    (push (car list) acc)))

(declaim (ftype (function (list fixnum) list) drop))
(defun drop (list n)
  (do ((list list (cdr list))
       (n n (1- n)))
      ((or (<= n 0) (null list)) list)))

(declaim (ftype (function ((function (t &rest t) t) list &rest list) list) filter-map))
(defun filter-map (fn list &rest more-list)
  (do ((lists (cons list more-list) (mapcar #'cdr lists))
       (acc nil))
      ((some #'null lists) (nreverse acc))
    (mylib.syntax:when-let ((item (apply fn (mapcar #'car lists))))
      (push item acc))))

(declaim (ftype (function (unsigned-byte &key (:start number) (:step number)) list) iota))
(defun iota (count &key (start 0) (step 1))
  (do ((count count (1- count))
       (start start (+ start step))
       (acc nil))
      ((<= count 0) (nreverse acc))
    (push start acc)))

(declaim (ftype (function (list &key (:test (function (t t) boolean))) list) unique))
(defun unique (list &key (test #'eql))
  (nreverse (reduce #'(lambda (acc x)
                        (if (funcall test x (car acc))
                            acc
                            (cons x acc)))
                    list
                    :initial-value nil)))

(declaim (ftype (function (list) list) flatten))
(defun flatten (list)
  (labels ((rec (list acc)
             (cond ((null list) acc)
                   ((listp (car list)) (rec (cdr list) (rec (car list) acc)))
                   (t (rec (cdr list) (cons (car list) acc))))))
    (nreverse (rec list nil))))

(declaim (ftype (function (list t) list) join))
(defun join (list separator)
  (when (null list)
    (return-from join nil))
  (do ((list (cdr list) (cdr list))
       (acc (list (car list)) (cons (car list) (cons separator acc))))
      ((null list) (nreverse acc))))

(defmacro with-collector ((&rest collectors) &body body)
  (let ((lists (mapcar #'(lambda (collector)
                           (declare (ignore collector))
                           (gensym))
                       collectors)))
    `(let ,(mapcar #'(lambda (list)
                       `(,list (list nil)))
                   lists)
       (flet ,(mapcar #'(lambda (collector list)
                          `(,collector (&optional (value nil supplied-p))
                            (when supplied-p
                              (tconc ,list value))
                            (car ,list)))
                      collectors
                      lists)
         ,@body))))
