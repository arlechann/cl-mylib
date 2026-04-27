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

(declaim (ftype (function ((or (function (unsigned-byte t t) t) symbol)
                           sequence
                           &rest t
                           &key (:key t) (:from-end t) (:start fixnum) (:end fixnum) (:initial-value t))
                          t)
                reduce-with-index))
(defun reduce-with-index (function sequence
                          &rest args
                          &key key from-end (start 0) end (initial-value nil ivp))
  (declare (ignore args))
  (let* ((end (or end (length sequence)))
         (transform (if key
                        key
                        #'identity)))
    (cond
      ((= start end)
       (if ivp
           initial-value
           (error "REDUCE-WITH-INDEX requires an initial value for an empty sequence.")))
      (from-end
       (let ((acc (if ivp
                      initial-value
                      (funcall transform (elt sequence (1- end))))))
         (loop for index downfrom (if ivp
                                      (1- end)
                                      (- end 2))
               to start
               do (setf acc (funcall function
                                     index
                                     acc
                                     (funcall transform (elt sequence index)))))
         acc))
      (t
       (let ((acc (if ivp
                      initial-value
                      (funcall transform (elt sequence start)))))
         (loop for index from (if ivp
                                  start
                                  (1+ start))
               below end
               do (setf acc (funcall function
                                     index
                                     acc
                                     (funcall transform (elt sequence index)))))
         acc)))))

(declaim (ftype (function ((or (function (unsigned-byte t) t) symbol)
                           sequence
                           &rest t
                           &key (:from-end t) (:start fixnum) (:end fixnum) (:key t))
                          (or null (cons fixnum t)))
                find-with-index))
(defun find-with-index (predicate sequence &rest args &key from-end start end key)
  (declare (ignore args))
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (transform (if key
                        key
                        #'identity)))
    (if from-end
        (loop for index downfrom (1- end) to start
              for item = (elt sequence index)
              when (funcall predicate index (funcall transform item))
                do (return (cons index item)))
        (loop for index from start below end
              for item = (elt sequence index)
              when (funcall predicate index (funcall transform item))
                do (return (cons index item))))))

(declaim (ftype (function ((or (function (t t) boolean) symbol)
                           sequence
                           &rest t
                           &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argopt))
(defun argopt (predicate sequence &rest args &key key from-end start end)
  (declare (ignore args))
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (transform (if key
                        key
                        #'identity)))
    (when (< start end)
      (if from-end
          (let* ((best-index (1- end))
                 (best-value (funcall transform (elt sequence best-index))))
            (loop for index downfrom (- end 2) to start
                  for value = (funcall transform (elt sequence index))
                  when (funcall predicate value best-value)
                    do (setf best-index index
                             best-value value))
            best-index)
          (let* ((best-index start)
                 (best-value (funcall transform (elt sequence best-index))))
            (loop for index from (1+ start) below end
                  for value = (funcall transform (elt sequence index))
                  when (funcall predicate value best-value)
                    do (setf best-index index
                             best-value value))
            best-index)))))

(declaim (ftype (function (sequence &rest t &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argmax))
(defun argmax (sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start end))
  (apply #'argopt #'> sequence args))

(declaim (ftype (function (sequence &rest t &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argmin))
(defun argmin (sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start end))
  (apply #'argopt #'< sequence args))
