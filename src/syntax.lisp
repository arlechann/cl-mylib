(in-package #:mylib.syntax)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 symbols)
     ,@body))

(defmacro do-array* (((&rest vars) (&rest arrays) &optional result) &body body)
  (let ((arrs (mapcar (lambda (x)
                        (declare (ignore x))
                        (gensym))
                      arrays))
        (index (gensym))
        (min-size (gensym)))
    `(let* (,@(mapcar #'list arrs arrays)
            (,min-size (apply #'min (mapcar #'array-total-size (list ,@arrs)))))
       (do ((,index 0 (1+ ,index)))
           ((= ,index ,min-size) ,result)
         (let ,(mapcar (lambda (var array-var)
                         `(,var (row-major-aref ,array-var ,index)))
                       vars arrs)
           (declare (ignorable ,@vars))
           ,@body)))))

(defmacro do-array ((var array &optional result) &body body)
  `(do-array* ((,var) (,array) ,result) ,@body))

(defmacro do-seq* (((&rest vars) (&rest sequences) &optional result) &body body)
  `(block nil
     (map nil
          (lambda ,vars
            (declare (ignorable ,@vars))
            ,@body)
          ,@sequences)
     ,result))

(defmacro do-seq ((var sequence &optional result) &body body)
  `(do-seq* ((,var) (,sequence) ,result) ,@body))

(defmacro named-let (name binds &body body)
  "名前付き再帰を、ローカル更新とジャンプによるループへ展開する。"
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

(defmacro nlet (name binds &body body)
  "NAMED-LET の alias。"
  `(named-let ,name ,binds ,@body))

(defmacro block-lambda (params &body body)
  `(lambda ,params
     (block nil
       ,@body)))

(defmacro named-lambda (name params &body body)
  `(labels ((,name ,params
              ,@body))
     #',name))

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

(defmacro and-let* (binds &body body)
  (labels ((expand (rest-binds)
             (if (null rest-binds)
                 (if body
                     `(progn ,@body)
                     t)
                 (destructuring-bind (var value-form) (car rest-binds)
                   `(let ((,var ,value-form))
                      (and ,var
                           ,(if (null (cdr rest-binds))
                                (if body
                                    `(progn ,@body)
                                    var)
                                (expand (cdr rest-binds)))))))))
    (expand binds)))

(defmacro when-let (binds &body body)
  `(let ,binds
     (when (and ,@(mapcar #'car binds) ,@body))))

(defmacro when-let* (binds &body body)
  `(let* ,binds
     (when (and ,@(mapcar #'car binds) ,@body))))

(defun %debug-print*-bindings-string (bindings)
  (with-output-to-string (stream)
    (loop for (name . value) in bindings
          for firstp = t then nil
          do (unless firstp
               (write-char #\Space stream))
             (format stream "~A=~S" name value))))

(defmacro debug-print (expr)
  (let ((value (gensym)))
    `(let ((,value ,expr))
       (fresh-line *error-output*)
       (format *error-output* "DEBUG: ~S => ~S~%" ',expr ,value)
       ,value)))

(defmacro debug-print* (vars expr)
  (let ((value (gensym))
        (bindings (gensym)))
    `(let* ((,value ,expr)
            (,bindings (list ,@(mapcar (lambda (var)
                                         `(cons ',var ,var))
                                       vars))))
       (fresh-line *error-output*)
       (format *error-output*
               "DEBUG*: ~A ~S => ~S~%"
               (%debug-print*-bindings-string ,bindings)
               ',expr
               ,value)
       ,value)))
