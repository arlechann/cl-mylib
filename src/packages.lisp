(in-package #:cl-user)

(defpackage #:mylib
  (:use #:cl))

(defpackage #:mylib.syntax
  (:use #:cl)
  (:export #:it
           #:self
           #:eval-always
           #:with-gensyms
           #:nlet
           #:while
           #:until
           #:aif
           #:alambda
           #:aprog1
           #:aand
           #:acond
           #:if-let
           #:if-let*
           #:when-let
           #:when-let*
           ))

(defpackage #:mylib.number
  (:use #:cl)
  (:export #:*eps*
           #:square
           #:clamp
           #:maxp
           #:minp
           #:maxf
           #:minf
           #:lerp
           #:approx=
           #:approx-zero-p
           #:approx<=
           #:approx>=
           ))

(defpackage #:mylib.sequence
  (:use #:cl)
  (:export #:sum
           #:sortf
           #:map-with-index
           #:map-into-with-index
           #:nmap
           #:nmap-with-index
           #:reduce-with-index
           #:find-with-index
           #:argopt
           #:argmax
           #:argmin
           ))

(defpackage #:mylib.list
  (:use #:cl)
  (:export #:ensure-car
           #:ensure-list
           #:xcons
           #:tconc
           #:lconc
           #:last1
           #:length=
           #:length<
           #:length>
           #:length<=
           #:length>=
           #:take
           #:drop
           #:filter-map
           #:iota
           #:unique
           #:flatten
           #:join
           #:with-collector
           ))

(defpackage #:mylib.algorithm
  (:use #:cl)
  (:export #:meguru-method
           #:binary-search
           #:lower-bound
           #:upper-bound
           ))

(defpackage #:mylib.amb
  (:use #:cl)
  (:export #:*failed*
           #:amb-reset
           #:amb
           #:amb-bind
           ))
