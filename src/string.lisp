(in-package #:mylib.string)

(defun strjoin (strings &key (spacer (string #\Newline)))
  (with-output-to-string (out)
    (dolist (item (mylib.list:join strings spacer))
      (write-string item out))))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))
