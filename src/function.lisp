(in-package #:mylib.function)

(defun do-nothing (&rest args)
  (declare (ignore args)))

(defun flip (function)
  #'(lambda (&rest args)
      (apply function (reverse args))))

(defun compose (&rest functions)
  (cond ((null functions) #'values)
        ((null (cdr functions)) (car functions))
        (t #'(lambda (&rest args)
               (labels ((rec (functions values)
                          (if (null functions)
                              (values-list values)
                              (multiple-value-call #'(lambda (&rest next-values)
                                                       (rec (cdr functions) next-values))
                                (apply (car functions) values)))))
                 (rec (reverse functions) args))))))

(mylib.syntax:eval-always
  (defun %compose-expansion (functions)
    (cond ((null functions) `#'values)
          ((null (cdr functions)) (car functions))
          (t (let ((args (gensym)))
               `#'(lambda (&rest ,args)
                    ,(reduce #'(lambda (form function)
                                 `(multiple-value-call ,function ,form))
                             (butlast functions)
                             :from-end t
                             :initial-value `(apply ,(car (last functions)) ,args))))))))

(define-compiler-macro compose (&whole form &rest functions)
  (if (every #'constantp functions)
      (%compose-expansion functions)
      form))

(mylib.syntax:eval-always
  (defun %predicate-chain-expansion (predicates terminal-value short-circuit-op)
    (cond ((null predicates) `(constantly ,terminal-value))
          ((null (cdr predicates)) (car predicates))
          (t (let ((args (gensym)))
               `#'(lambda (&rest ,args)
                    (,short-circuit-op
                     ,@(mapcar #'(lambda (predicate)
                                   `(apply ,predicate ,args))
                               predicates))))))))

(defun conjoin (&rest predicates)
  (cond ((null predicates) (constantly t))
        ((null (cdr predicates)) (car predicates))
        (t #'(lambda (&rest args)
               (dolist (predicate predicates t)
                 (unless (apply predicate args)
                   (return nil)))))))

(defun disjoin (&rest predicates)
  (cond ((null predicates) (constantly nil))
        ((null (cdr predicates)) (car predicates))
        (t #'(lambda (&rest args)
               (dolist (predicate predicates nil)
                 (when (apply predicate args)
                   (return t)))))))

(define-compiler-macro conjoin (&whole form &rest predicates)
  (if (every #'constantp predicates)
      (%predicate-chain-expansion predicates t 'and)
      form))

(define-compiler-macro disjoin (&whole form &rest predicates)
  (if (every #'constantp predicates)
      (%predicate-chain-expansion predicates nil 'or)
      form))

(mylib.syntax:eval-always
  (defun %pa-placeholder-index (form)
    (when (keywordp form)
      (let ((name (symbol-name form)))
        (when (and (> (length name) 1)
                   (char= (char name 0) #\$))
          (multiple-value-bind (value position)
              (parse-integer name :start 1 :junk-allowed t)
            (when (= position (length name))
              value)))))))

(mylib.syntax:eval-always
  (defun %pa-rest-placeholder-p (form)
    (and (keywordp form)
         (string= (symbol-name form) "$@"))))

(mylib.syntax:eval-always
  (defun %pa-max-index (forms)
    (loop for form in forms
          for index = (%pa-placeholder-index form)
          when index maximize index into max-index
          finally (return (or max-index -1)))))

(mylib.syntax:eval-always
  (defun %pa-lambda-vars (max-index)
    (loop repeat (1+ max-index) collect (gensym "ARG"))))

(mylib.syntax:eval-always
  (defun %pa-call-expression (function-form forms expanded-forms restp)
    (if restp
        `(apply ,function-form
                (append
                 ,@(mapcar #'(lambda (form expanded-form)
                               (if (%pa-rest-placeholder-p form)
                                   expanded-form
                                   `(list ,expanded-form)))
                           forms
                           expanded-forms)))
        `(funcall ,function-form ,@expanded-forms))))

(mylib.syntax:eval-always
  (defun %pa-lambda-form (vars rest-var call-form)
    `#'(lambda ,(append vars (if rest-var `(&rest ,rest-var) nil))
         ,call-form)))

(mylib.syntax:eval-always
  (defun %pa-form (function forms)
    (let* ((max-index (%pa-max-index forms))
           (vars (%pa-lambda-vars max-index))
           (restp (find-if #'%pa-rest-placeholder-p forms))
           (rest-var (and restp (gensym "REST")))
           (expanded-forms
             (mapcar #'(lambda (form)
                         (let ((index (%pa-placeholder-index form)))
                           (cond (index (nth index vars))
                                 ((%pa-rest-placeholder-p form) rest-var)
                                 (t form))))
                     forms))
           (call-form (%pa-call-expression function forms expanded-forms restp)))
      (%pa-lambda-form vars rest-var call-form))))

(mylib.syntax:eval-always
  (defun %pa*-form (function forms)
    (let* ((max-index (%pa-max-index forms))
           (vars (%pa-lambda-vars max-index))
           (restp (find-if #'%pa-rest-placeholder-p forms))
           (rest-var (and restp (gensym "REST")))
           (bindings nil)
           (function-var (gensym "FUNCTION"))
           (expanded-forms
             (mapcar #'(lambda (form)
                         (let ((index (%pa-placeholder-index form)))
                           (cond (index (nth index vars))
                                 ((%pa-rest-placeholder-p form) rest-var)
                                 (t
                                  (let ((temp (gensym "FIXED")))
                                    (push (list temp form) bindings)
                                    temp)))))
                     forms))
           (call-form (%pa-call-expression function-var forms expanded-forms restp)))
      (push (list function-var function) bindings)
      `(let ,(nreverse bindings)
         ,(%pa-lambda-form vars rest-var call-form)))))

(defmacro pa (function &rest forms)
  "FUNCTION と :$0, :$1, ... , :$@ プレースホルダから関数を構築する。
プレースホルダでない式は、返された関数の呼び出し時に評価される。"
  (%pa-form function forms))

(defmacro pa* (function &rest forms)
  "PA と同様に関数を構築するが、プレースホルダでない式は生成時に評価して束縛する。"
  (%pa*-form function forms))
