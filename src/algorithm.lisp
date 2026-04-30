(in-package #:mylib.algorithm)

(declaim (ftype (function (integer integer (function (integer) boolean)) integer) meguru-method))
(defun meguru-method (ok ng predicate)
  (loop until (<= (abs (- ok ng)) 1)
        for middle = (floor (+ ok ng) 2)
        if (funcall predicate middle)
          do (setf ok middle)
        else
          do (setf ng middle)
        finally (return ok)))

(declaim (ftype (function (real real (function (real) boolean)
                           &key (:eps real) (:max-iteration fixnum))
                          real)
                binary-search))
(defun binary-search (ok ng predicate &key (eps mylib.number:*eps*) (max-iteration 300))
  (loop with iteration = 0
        while (and (> (abs (- ok ng)) eps)
                   (< iteration max-iteration))
        for middle = (/ (+ ok ng) 2)
        if (funcall predicate middle)
          do (setf ok middle)
        else
          do (setf ng middle)
        do (incf iteration)
        finally (return ok)))

(declaim (ftype (function (vector real &key (:compare (function (real real) boolean)) (:start fixnum) (:end fixnum)) fixnum) find-bound))
(defun find-bound (vector element &key compare (start 0) end)
  (meguru-method (or end (length vector))
                 (1- start)
                 #'(lambda (index)
                     (funcall compare element (aref vector index)))))

(declaim (ftype (function (vector real &key (:start fixnum) (:end fixnum)) fixnum) lower-bound))
(defun lower-bound (vector element &rest args &key (start 0) end)
  (declare (ignore start end))
  (apply #'find-bound
         vector
         element
         :compare #'<=
         args))

(declaim (ftype (function (vector real &key (:start fixnum) (:end fixnum)) fixnum) upper-bound))
(defun upper-bound (vector element &rest args &key (start 0) end)
  (declare (ignore start end))
  (apply #'find-bound
         vector
         element
         :compare #'<
         args))
