(in-package #:mylib.sequence)

(declaim (inline sort))

(declaim (ftype (function (sequence) number) sum))
(defun sum (sequence) (reduce #'+ sequence :initial-value 0))

(define-modify-macro sortf (compare &rest args)
  (lambda (sequence compare &rest args &key key)
    (declare (ignore key))
    (apply #'sort sequence copare args)))

(declaim (ftype (function ((or cons symbol class)
                           (or (function (unsigned-byte t &rest t) t) symbol)
                           sequence
                           &rest sequence)
                          sequence)
                map-with-index))
(defun map-with-index (result-type fn sequence &rest more-sequences)
  (let ((index 0))
    (apply #'map
           result-type
           #'(lambda (&rest args)
               (prog1 (apply fn index args)
                 (incf index)))
           sequence
           more-sequences)))

(declaim (ftype (function (sequence
                           (or (function (unsigned-byte t &rest t) t) symbol)
                           &rest sequence))
                map-into-with-index))
(defun map-into-with-index (result-sequence fn &rest sequences)
  (let ((index 0))
    (apply #'map-into
           result-sequence
           #'(lambda (&rest args)
               (prog1 (apply fn index args)
                 (incf index)))
           sequences)))

(declaim (ftype (function ((or (function (t &rest t) t) symbol)
                           sequence
                           &rest sequence)
                          sequence)
                nmap))
(defun nmap (fn sequence &rest more-sequences)
  (apply #'map-into sequence fn sequence more-sequences))

(declaim (ftype (function ((or (function (unsigned-byte t &rest t) t) symbol)
                           sequence
                           &rest sequence)
                          sequence)
                nmap-with-index))
(defun nmap-with-index (fn sequence &rest more-sequences)
  (let ((index 0))
    (apply #'map-into
           sequence
           #'(lambda (&rest args)
               (prog1 (apply fn index args)
                 (incf index)))
           sequence
           more-sequences)))

(defun get-start-index (sequence &key from-end start end (initial-value nil ivp))
  (if from-end
      (- (or end (length sequence))
         (if ivp 1 2))
      (+ (or start 0)
         (if ivp 0 1))))

(declaim (ftype (function ((or (function (unsigned-byte t t) t) symbol)
                           sequence
                           &rest t
                           &key (:key t) (:from-end t) (:start fixnum) (:end fixnum) (:initial-value t))
                          t)
                reduce-with-index))
(defun reduce-with-index (function sequence
                          &rest args
                          &key key from-end start end initial-value)
  (declare (ignore key start end initial-value))
  (let ((index (apply #'get-start-index args))
        (delta (if from-end -1 1)))
    (apply #'reduce
           #'(lambda (x y)
               (prog1 (funcall function index x y)
                 (incf index delta)))
           sequence
           args)))

(declaim (ftype (function ((or (function (unsigned-byte t) t) symbol)
                           sequence
                           &rest t
                           &key (:from-end t) (:start fixnum) (:end fixnum) (:key t))
                          (or null (cons fixnum t)))
                find-with-index))
(defun find-with-index (predicate sequence &rest args &key from-end start end key)
  (declare (ignore start end key))
  (let ((index (apply #'get-start-index args))
        (delta (if from-end -1 1)))
    (let* ((last-index 0)
           (found (apply #'find-if
                         #'(lambda (x)
                             (prog1 (funcall predicate index x)
                               (setf last-index index)
                               (incf index delta)))
                         sequence
                         args)))
      (and found (cons last-index found)))))

(declaim (ftype (function ((or (function (t t) boolean) symbol)
                           sequence
                           &rest t
                           &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argopt))
(defun argopt (predicate sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start end))
  (cdr (apply #'reduce-with-index
              #'(lambda (i acc x)
                  (if (or (null acc)
                          (funcall predicate x (car acc)))
                      (cons x i)
                      acc))
              sequence
              :initial-value nil
              args)))

(declaim (ftype (function (sequence &rest t &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argmax))
(defun argmax (sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start  end))
  (apply #'argopt #'> sequence args))

(declaim (ftype (function (sequence &rest t &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argmin))
(defun argmin (sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start end))
  (apply #'argopt #'< sequence args))
