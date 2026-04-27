(in-package #:mylib.number)

(declaim (ftype (function (t) t) square))
(defun square (x) (* x x))

(declaim (ftype (function (real real real) real) clamp))
(defun clamp (x low high) (max low (min x high)))

(declaim (ftype (function (real &rest real) boolean) maxp))
(defun maxp (x &rest args)
  (or (null args)
      (> x (apply #'max args))))

(declaim (ftype (function (real &rest real) boolean) minp))
(defun minp (x &rest args)
  (or (null args)
      (< x (apply #'min args))))

(defmacro maxf (place &rest args)
  (multiple-value-bind (vars vals tmp-vars setter reader)
      (get-setf-expansion place)
    (let ((max-value (gensym)))
      `(let* (,@(mapcar #'list vars vals)
              (,max-value (max ,reader ,@args)))
         (let ((,(car tmp-vars) ,max-value))
           ,setter)))))

(defmacro minf (place &rest args)
  (multiple-value-bind (vars vals tmp-vars setter reader)
      (get-setf-expansion place)
    (let ((min-value (gensym)))
      `(let* (,@(mapcar #'list vars vals)
              (,min-value (min ,reader ,@args)))
         (let ((,(car tmp-vars) ,min-value))
           ,setter)))))

(declaim (ftype (function (real real real) real) lerp))
(defun lerp (a b ratio)
  (+ a (* ratio (- b a))))

(declaim (type real *eps*))
(defvar *eps* 1d-12)

(declaim (ftype (function (real real &key (:eps real)) boolean) approx=))
(defun approx= (x y &key (eps *eps*))
  (<= (abs (- x y)) eps))

(declaim (ftype (function (real &key (:eps real)) boolean) approx-zero-p))
(defun approx-zero-p (x &key (eps *eps*))
  (<= (abs x) eps))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx<=))
(defun approx<= (x y &key (eps *eps*))
  (or (< x y)
      (approx= x y :eps eps)))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx>=))
(defun approx>= (x y &key (eps *eps*))
  (or (> x y)
      (approx= x y :eps eps)))
