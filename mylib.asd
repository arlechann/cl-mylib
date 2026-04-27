(defsystem "mylib"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "syntax")
                 (:file "number")
                 (:file "sequence")
                 (:file "list")
                 (:file "algorithm")
                 (:file "amb"))))
  :description ""
  :in-order-to ((test-op (test-op "mylib/tests"))))

(defsystem "mylib/tests"
  :author ""
  :license ""
  :depends-on ("mylib"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "syntax"))))
  :description "Test system for mylib"
  :perform (test-op (op c) (symbol-call :rove :run c)))
