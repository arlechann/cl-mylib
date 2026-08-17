(in-package #:cl-user)

(defpackage #:mylib/tests/syntax
  (:use #:cl
        #:mylib.syntax
        #:rove))

(defpackage #:mylib/tests/function
  (:use #:cl
        #:mylib.function
        #:rove))

(defpackage #:mylib/tests/number
  (:use #:cl
        #:mylib.number
        #:rove))

(defpackage #:mylib/tests/sequence
  (:use #:cl
        #:mylib.sequence
        #:rove))

(defpackage #:mylib/tests/list
  (:use #:cl
        #:mylib.list
        #:rove))

(defpackage #:mylib/tests/string
  (:use #:cl
        #:mylib.string
        #:rove))

(defpackage #:mylib/tests/lazy
  (:use #:cl
        #:mylib.lazy
        #:rove))

(defpackage #:mylib/tests/list-queue
  (:use #:cl
        #:list-queue
        #:rove))

(defpackage #:mylib/tests/algorithm
  (:use #:cl
        #:mylib.algorithm
        #:rove))

(defpackage #:mylib/tests/amb
  (:use #:cl
        #:mylib.amb
        #:rove))
