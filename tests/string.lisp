(in-package #:mylib/tests/string)

(deftest string-helpers
  (ok (equal "a,b,c" (strjoin '("a" "b" "c") :spacer ",")))
  (ok (equal "abc" (strjoin '("a" "b" "c") :spacer "")))
  (ok (equal (format nil "a~%b~%c")
             (strjoin '("a" "b" "c"))))
  (ok (equal "abc"
             (trim-whitespace
              (concatenate 'string (string #\Space) (string #\Tab) "abc"
                           (string #\Newline) (string #\Return))))))
