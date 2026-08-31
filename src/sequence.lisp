(in-package #:mylib.sequence)

(declaim (inline sort))

(declaim (ftype (function (sequence) number) sum))
(defun sum (sequence) (reduce #'+ sequence :initial-value 0))

(define-modify-macro sortf (compare &rest args)
  (lambda (sequence compare &rest args &key key)
    (declare (ignore key))
    (apply #'sort sequence copare args)))

(define-modify-macro reversef () reverse)

(define-modify-macro nreversef () nreverse)

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

(declaim (ftype (function ((or cons symbol class)
                           (integer 1 *)
                           (or (function (t &rest t) t) symbol)
                           sequence)
                          sequence)
                window-map))
(let ((t-fn (constantly t)))
  (defun window-map (result-type window-size fn sequence)
    (let ((len (length sequence)))
      (declare (ignorable len))
      (when (> window-size len)
        (error "WINDOW-MAP: window-size ~D exceeds sequence length ~D." window-size len))
      (let ((result (let ((index 0)
                          (queue (mylib.list-queue:make-list-queue)))
                      (map result-type
                           (lambda (e)
                             (mylib.list-queue:list-queue-enqueue queue e)
                             (incf index)
                             (when (>= index window-size)
                               (prog1 (apply fn (mylib.list-queue:list-queue-raw queue))
                                 (mylib.list-queue:list-queue-dequeue queue))))
                           sequence))))
        (and result
             (delete-if t-fn
                        result
                        :end (1- window-size)))))))

(declaim (ftype (function ((integer 1 *)
                           (or (function (t &rest t) t) symbol)
                           sequence)
                          sequence)
                window-nmap))
(let ((t-fn (constantly t)))
  (defun window-nmap (window-size fn sequence)
    (let ((len (length sequence)))
      (declare (ignorable len))
      (when (> window-size len)
        (error "WINDOW-NMAP: window-size ~D exceeds sequence length ~D." window-size len))
      (delete-if t-fn
                 (let ((index 0)
                       (queue (mylib.list-queue:make-list-queue)))
                   (nmap
                    (lambda (e)
                      (mylib.list-queue:list-queue-enqueue queue e)
                      (incf index)
                      (when (>= index window-size)
                        (prog1 (apply fn (mylib.list-queue:list-queue-raw queue))
                          (mylib.list-queue:list-queue-dequeue queue))))
                    sequence))
                 :end (1- window-size)))))

(declaim (ftype (function (sequence &key (:test (or symbol (function (t t) t))))
                          list)
                run-length-encode))
(defun run-length-encode (sequence &key (test #'eql))
  (let ((prev '#.(gensym))
        (acc nil))
    (map nil
         (lambda (e)
           (if (funcall test e prev)
               (incf (cdar acc))
               (push (cons e 1) acc))
           (setf prev e))
         sequence)
    (nreverse acc)))

(declaim (ftype (function (&rest t) vector) vector*))
(defun vector* (&rest contents)
  (make-array (length contents)
              :initial-contents contents
              :adjustable t
              :fill-pointer t))

(declaim (ftype (function (vector &key (:start unsigned-byte) (:end unsigned-byte))
                          vector)
                displaced-subvec))
(defun displaced-subvec (vector &key (start 0) end)
  (make-array (- (or end (length vector)) start)
              :element-type (array-element-type vector)
              :displaced-to vector
              :displaced-index-offset start))
