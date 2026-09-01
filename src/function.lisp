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
  (defun %placeholder-index (form)
    (when (keywordp form)
      (let ((name (symbol-name form)))
        (when (and (> (length name) 1)
                   (char= (char name 0) #\$))
          (multiple-value-bind (value position)
              (parse-integer name :start 1 :junk-allowed t)
            (when (= position (length name))
              value)))))))

(mylib.syntax:eval-always
  (defun %splicing-rest-placeholder-p (form)
    (and (keywordp form)
         (string= (symbol-name form) "$@"))))

(mylib.syntax:eval-always
  (defun %list-rest-placeholder-p (form)
    (and (keywordp form)
         (string= (symbol-name form) "$*"))))

(mylib.syntax:eval-always
  (defun %quoted-or-function-symbol-form-p (form)
    (or (and (symbolp (car form))
             (member (car form) '(quote quasiquote) :test #'eq))
        (and (eq (car form) 'function)
             (symbolp (cadr form))))))

(mylib.syntax:eval-always
  (defun %pa-max-index (forms)
    (let ((foundp nil)
          (max-index 0))
      (dolist (form forms (if foundp max-index -1))
        (let ((index (%placeholder-index form)))
          (when index
            (setf foundp t
                  max-index (max max-index index))))))))

(mylib.syntax:eval-always
  (defun %placeholder-lambda-vars (max-index)
    (loop repeat (1+ max-index) collect (gensym "ARG"))))

(mylib.syntax:eval-always
  (defun %walk-placeholder-form (form atom-fn quote-fn combine-fn)
    (labels ((rec (form)
               (cond ((atom form)
                      (funcall atom-fn form))
                     ((%quoted-or-function-symbol-form-p form)
                      (funcall quote-fn form))
                     (t
                      (funcall combine-fn
                               (rec (car form))
                               (rec (cdr form)))))))
      (rec form))))

(mylib.syntax:eval-always
  (defun %placeholder-max-index-in-form (form)
    (%walk-placeholder-form form
                            (lambda (form)
                              (or (%placeholder-index form) -1))
                            (lambda (form)
                              (declare (ignore form))
                              -1)
                            #'max)))

(mylib.syntax:eval-always
  (defun %placeholder-in-form-p (form predicate)
    (%walk-placeholder-form form
                            predicate
                            (lambda (form)
                              (declare (ignore form))
                              nil)
                            (lambda (left right)
                              (or left right)))))

(mylib.syntax:eval-always
  (defun %expand-placeholders-in-form (form vars &optional rest-var)
    (%walk-placeholder-form
     form
     (lambda (form)
       (let ((index (%placeholder-index form)))
         (cond (index (nth index vars))
               ((%list-rest-placeholder-p form) rest-var)
               (t form))))
     #'identity
     #'cons)))

(mylib.syntax:eval-always
  (defun %pa-call-expression (function-form forms expanded-forms restp)
    (if restp
        `(apply ,function-form
                (append
                 ,@(mapcar (lambda (form expanded-form)
                             (if (%splicing-rest-placeholder-p form)
                                 expanded-form
                                 `(list ,expanded-form)))
                           forms
                           expanded-forms)))
        `(funcall ,function-form ,@expanded-forms))))

(mylib.syntax:eval-always
  (defun %pa-rest-placeholder-used-p (forms)
    (find-if (lambda (form)
               (or (%splicing-rest-placeholder-p form)
                   (%list-rest-placeholder-p form)))
             forms)))

(mylib.syntax:eval-always
  (defun %placeholder-lambda-form (vars rest-var call-form)
    `#'(lambda ,(append vars (if rest-var `(&rest ,rest-var) nil))
         ,call-form)))

(mylib.syntax:eval-always
  (defun %placeholder-argument-context (forms)
    (let* ((max-index (%pa-max-index forms))
           (vars (%placeholder-lambda-vars max-index))
           (restp (%pa-rest-placeholder-used-p forms))
           (rest-var (and restp (gensym "REST"))))
      (values vars rest-var (find-if #'%splicing-rest-placeholder-p forms)))))

(mylib.syntax:eval-always
  (defun %placeholder-body-context (form rest-placeholder-p)
    (let* ((max-index (%placeholder-max-index-in-form form))
           (vars (%placeholder-lambda-vars max-index))
           (restp (%placeholder-in-form-p form rest-placeholder-p))
           (rest-var (and restp (gensym "REST"))))
      (values vars rest-var))))

(mylib.syntax:eval-always
  (defun %expand-placeholder-argument-forms (forms vars rest-var
                                             &optional fixed-forms)
    (loop for form in forms
          for fixed-form in (or fixed-forms (make-list (length forms)))
          collect
          (let ((index (%placeholder-index form)))
            (cond (index (nth index vars))
                  ((or (%splicing-rest-placeholder-p form)
                       (%list-rest-placeholder-p form))
                   rest-var)
                  (fixed-form fixed-form)
                  (t form))))))

(mylib.syntax:eval-always
  (defun %fixed-bindings-and-forms (forms)
    (let ((bindings nil))
      (values
       (mapcar (lambda (form)
                 (if (or (%placeholder-index form)
                         (%splicing-rest-placeholder-p form)
                         (%list-rest-placeholder-p form))
                     nil
                     (let ((temp (gensym "FIXED")))
                       (push (list temp form) bindings)
                       temp)))
               forms)
       (nreverse bindings)))))

(mylib.syntax:eval-always
  (defun %lambda-list-bound-vars (lambda-list)
    (labels ((rec (item)
               (cond ((null item) nil)
                     ((symbolp item)
                      (unless (member item lambda-list-keywords :test #'eq)
                        (list item)))
                     ((consp item)
                      (case (car item)
                        ((&whole &environment)
                         (rec (cadr item)))
                        (otherwise
                         (append (rec (car item))
                                 (when (and (consp (cdr item))
                                            (consp (cddr item)))
                                   (rec (caddr item)))))))
                     (t nil))))
      (loop for item in lambda-list append (rec item)))))

(mylib.syntax:eval-always
  (defun %fixed-bindings-and-form (form)
    (let ((bindings nil))
      (labels ((rec (form headp &optional bound-vars)
                 (cond ((atom form)
                        (cond ((or (%placeholder-index form)
                                   (%list-rest-placeholder-p form)
                                   (null form)
                                   (member form bound-vars :test #'eq)
                                   (and headp (symbolp form)))
                               form)
                              (t
                               (let ((temp (gensym "FIXED")))
                                 (push (list temp form) bindings)
                                 temp))))
                       ((and (eq (car form) 'lambda)
                             (consp (cdr form)))
                        (let ((lambda-vars (%lambda-list-bound-vars (cadr form))))
                          (list* 'lambda
                                 (cadr form)
                                 (mapcar (lambda (subform)
                                           (rec subform t (append lambda-vars bound-vars)))
                                         (cddr form)))))
                       ((and (eq (car form) 'function)
                             (consp (cdr form))
                             (consp (cadr form))
                             (eq (caadr form) 'lambda))
                        (list 'function (rec (cadr form) t bound-vars)))
                       ((%quoted-or-function-symbol-form-p form)
                        form)
                       (t
                        (cons (rec (car form) t bound-vars)
                              (rec-tail (cdr form) bound-vars)))))
               (rec-tail (tail bound-vars)
                 (if (atom tail)
                     (rec tail nil bound-vars)
                     (cons (rec (car tail) nil bound-vars)
                           (rec-tail (cdr tail) bound-vars)))))
        (values (rec form t)
                (nreverse bindings))))))

(mylib.syntax:eval-always
  (defun %placeholder-function-form (function-form forms vars rest-var splicing-restp
                                     expanded-forms &optional bindings)
    (let* ((call-form (%pa-call-expression function-form
                                           forms
                                           expanded-forms
                                           splicing-restp))
           (lambda-form (%placeholder-lambda-form vars rest-var call-form)))
      (if bindings
          `(let ,bindings
             ,lambda-form)
          lambda-form))))

(mylib.syntax:eval-always
  (defun %pa-form (function forms)
    (multiple-value-bind (vars rest-var splicing-restp)
        (%placeholder-argument-context forms)
      (%placeholder-function-form function
                                  forms
                                  vars
                                  rest-var
                                  splicing-restp
                                  (%expand-placeholder-argument-forms forms vars rest-var)))))

(mylib.syntax:eval-always
  (defun %pa*-form (function forms)
    (multiple-value-bind (vars rest-var splicing-restp)
        (%placeholder-argument-context forms)
      (multiple-value-bind (fixed-forms bindings)
          (%fixed-bindings-and-forms forms)
        (let* ((function-var (gensym "FUNCTION"))
               (expanded-forms
                 (%expand-placeholder-argument-forms forms vars rest-var fixed-forms)))
          (%placeholder-function-form function-var
                                      forms
                                      vars
                                      rest-var
                                      splicing-restp
                                      expanded-forms
                                      (append bindings (list (list function-var function)))))))))

(mylib.syntax:eval-always
  (defun %fn-form (body)
    (let ((form (if (null (cdr body))
                    (car body)
                    `(progn ,@body))))
      (multiple-value-bind (vars rest-var)
          (%placeholder-body-context form #'%list-rest-placeholder-p)
        (let ((expanded-form (%expand-placeholders-in-form form vars rest-var)))
          (%placeholder-lambda-form vars rest-var expanded-form))))))

(mylib.syntax:eval-always
  (defun %fn*-form (body)
    (let ((form (if (null (cdr body))
                    (car body)
                    `(progn ,@body))))
      (multiple-value-bind (vars rest-var)
          (%placeholder-body-context form #'%list-rest-placeholder-p)
        (multiple-value-bind (fixed-form bindings)
            (%fixed-bindings-and-form form)
          (let ((expanded-form (%expand-placeholders-in-form fixed-form vars rest-var)))
            (if bindings
                `(let ,bindings
                   ,(%placeholder-lambda-form vars rest-var expanded-form))
                (%placeholder-lambda-form vars rest-var expanded-form))))))))

(defmacro pa (function &rest forms)
  "プレースホルダを使って FUNCTION に部分適用するマクロ。
:$0, :$1, ... , :$@, :$* プレースホルダを使う。
:$@ は残り引数をその場で展開し、:$* は残り引数リストを 1 引数として渡す。
プレースホルダでない式は、返された関数の呼び出し時に評価される。
例:
  (funcall (pa #'list :$1 :$0) 'a 'b) => (B A)
  (funcall (pa #'list :$0 :$*) 1 2 3) => (1 (2 3))
  (funcall (pa #'list :$0 :$@) 1 2 3) => (1 2 3)"
  (%pa-form function forms))

(defmacro pa* (function &rest forms)
  "PA と同様に関数を構築する。
:$@ は残り引数をその場で展開し、:$* は残り引数リストを 1 引数として渡す。
プレースホルダでない式は生成時に評価して束縛する。
例:
  (let ((x 10))
    (let ((f (pa* #'+ x :$0)))
      (setf x 100)
      (funcall f 3)))
  => 13"
  (%pa*-form function forms))

(defmacro fn (&body body)
  "暗黙的引数を扱う関数構築マクロ。
BODY 内の :$0, :$1, ... プレースホルダからラムダ式を構築する。
:$* は残り引数リストを 1 引数として参照する。
例:
  (mapcar (fn (* (1+ :$0) 2)) '(0 1 2)) => (2 4 6)
  (funcall (fn (list :$0 :$*)) 1 2 3 4) => (1 (2 3 4))"
  (%fn-form body))

(defmacro fn* (&body body)
  "FN と同様にラムダ式を構築する。
:$* は残り引数リストを 1 引数として参照する。
プレースホルダでない式は生成時に評価して束縛する。
例:
  (let ((x 10))
    (let ((f (fn* (+ x :$0))))
      (setf x 100)
      (funcall f 3)))
  => 13"
  (%fn*-form body))
