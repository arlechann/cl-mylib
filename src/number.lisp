(in-package #:mylib.number)

(declaim (ftype (function (t &optional (function (t t) t)) t) square))
(defun square (x &optional (op #'*)) (funcall op x x))

(declaim (ftype (function (t &optional (function (t t) t)) t) cube))
(defun cube (x &optional (op #'*)) (funcall op (funcall op x x) x))

(declaim (ftype (function (t unsigned-byte &key (:op (function (t t) t)) (:identity t)) t) pow))
(defun pow (base power &key (op #'*) (identity 1))
  (mylib.syntax:named-let rec ((base base) (power power) (ret identity))
    (cond ((zerop power) ret)
          ((oddp power)
           (rec (square base op) (floor power 2) (funcall op ret base)))
          (t (rec (square base op) (floor power 2) ret)))))

(declaim (ftype (function (number number) (real 0 *)) diff))
(defun diff (a b) (abs (- a b)))

(declaim (ftype (function (unsigned-byte) unsigned-byte) next-pow2))
(defun next-pow2 (n)
  (declare (type unsigned-byte n))
  (if (zerop (logand n (1- n))) n
      (mylib.syntax:named-let rec ((n n) (acc 1))
        (if (zerop n)
            acc
            (rec (ash n -1) (ash acc 1))))))

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

(define-modify-macro maxf (&rest args) max)

(define-modify-macro minf (&rest args) min)

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
