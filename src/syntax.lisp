(in-package #:mylib.syntax)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 symbols)
     ,@body))

(defmacro nlet (name binds &body body)
  (let ((tag (gensym))
        (vars (mapcar #'car binds))
        (vals (mapcar #'cadr binds))
        (tmp-vars (mapcar #'(lambda (bind)
                              (declare (ignore bind))
                              (gensym))
                          binds))
        (rec-args (mapcar #'(lambda (bind)
                              (declare (ignore bind))
                              (gensym))
                          binds)))
    `(block ,name
       (let ,(mapcar #'list tmp-vars vals)
         (tagbody
            ,tag
            (let ,(mapcar #'list vars tmp-vars)
              (return-from ,name
                (macrolet ((,name ,rec-args
                             `(progn (psetq ,@(mapcan #'list ',tmp-vars (list ,@rec-args)))
                                     (go ,',tag))))
                  ,@body))))))))

(defmacro until (test &body body)
  `(do () (,test) ,@body))

(defmacro while (test &body body)
  `(until (not ,test) ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body)) #'self))

(defmacro aprog1 (result &body body)
  `(let ((it ,result)) (prog1 it ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) 't)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((head (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car head)))
           (if ,sym
               (let ((it ,sym))
                 ,@(cdr head))
               (acond ,@(cdr clauses)))))))

(defmacro if-let (binds then &optional else)
  `(let ,binds
     (if (and ,@(mapcar #'car binds)) ,then ,else)))

(defmacro if-let* (binds then &optional else)
  `(let* ,binds
     (if (and ,@(mapcar #'car binds)) ,then ,else)))

(defmacro when-let (binds &body body)
  `(let ,binds
     (when (and ,@(mapcar #'car binds) ,@body))))

(defmacro when-let* (binds &body body)
  `(let* ,binds
     (when (and ,@(mapcar #'car binds) ,@body))))
