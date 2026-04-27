(in-package #:mylib.amb)

(defvar *stack* nil)

(defvar *failed* nil)

(defun amb-reset ()
  (setf *stack* nil))

(defun fail ()
  (if (null *stack*)
      *failed*
      (funcall (pop *stack*))))

(defmacro amb (&rest options)
  (if (null options)
      '(fail)
      `(progn ,@(mapcar #'(lambda (option)
                            `(push #'(lambda () ,option)
                                   *stack*))
                        (reverse (cdr options)))
              ,(car options))))

(defmacro amb-bind (var options &body body)
  `(amb-bind-fn #'(lambda (,var) ,@body) ,options))

(defun amb-bind-fn (cont options)
  (if (null options)
      (fail)
      (progn (push #'(lambda ()
                       (amb-bind-fn cont (cdr options)))
                   *stack*)
             (funcall cont (car options)))))
