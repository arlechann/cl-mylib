(defsystem "mylib"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "syntax")
                 (:file "function")
                 (:file "number")
                 (:file "list-queue")
                 (:file "sequence")
                 (:file "list")
                 (:file "string")
                 (:file "lazy")
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
                 (:file "syntax")
                 (:file "function")
                 (:file "number")
                 (:file "sequence")
                 (:file "list")
                 (:file "string")
                 (:file "lazy")
                 (:file "list-queue")
                 (:file "algorithm")
                 (:file "amb"))))
  :description "Test system for mylib"
  :perform (test-op (op c) (symbol-call :rove :run c)))
