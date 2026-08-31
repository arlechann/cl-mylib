(in-package #:cl-user)

(uiop:define-package #:mylib
  (:use #:cl)
  (:use-reexport #:mylib.syntax
                 #:mylib.function
                 #:mylib.number
                 #:mylib.sequence
                 #:mylib.list
                 #:mylib.string
                 #:mylib.lazy
                 #:mylib.algorithm
                 #:mylib.amb))

(defpackage #:mylib.syntax
  (:use #:cl)
  (:export #:it
           #:self
           #:eval-always
           #:with-gensyms
           #:do-array
           #:do-array*
           #:do-seq
           #:do-seq*
           #:nlet
           #:block-lambda
           #:named-lambda
           #:while
           #:until
           #:aif
           #:alambda
           #:aprog1
           #:aand
           #:acond
           #:if-let
           #:if-let*
           #:and-let*
           #:when-let
           #:when-let*
           #:debug-print
           #:debug-print*
           ))

(defpackage #:mylib.function
  (:use #:cl)
  (:export #:do-nothing
           #:flip
           #:compose
           #:conjoin
           #:disjoin
           #:pa
           #:pa*
           #:fn
           #:fn*))

(defpackage #:mylib.number
  (:use #:cl)
  (:export #:*eps*
           #:cube
           #:pow
           #:diff
           #:next-pow2
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
           #:reversef
           #:nreversef
           #:map-with-index
           #:map-into-with-index
           #:nmap
           #:nmap-with-index
           #:reduce-with-index
           #:find-with-index
           #:argopt
           #:argmax
           #:argmin
           #:window-map
           #:window-nmap
           #:run-length-encode
           #:vector*
           #:displaced-subvec))

(defpackage #:mylib.list
  (:use #:cl)
  (:export #:ensure-car
           #:ensure-list
           #:xcons
           #:mapc-with-index
           #:mapcar-with-index
           #:mapcan-with-index
           #:mapl-with-index
           #:maplist-with-index
           #:mapcon-with-index
           #:tconc
           #:lconc
           #:singlep
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
           #:longerp
           #:longer
           #:unfold
           #:unique
           #:chunks
           #:flatten
           #:join
           #:with-collector))

(defpackage #:mylib.string
  (:use #:cl)
  (:export #:strjoin
           #:trim-whitespace))

(defpackage #:mylib.lazy
  (:use #:cl)
  (:export #:delay
           #:force))

(defpackage #:list-queue
  (:use #:cl)
  (:export #:make-list-queue
           #:list-queue-empty-p
           #:list-queue-peek
           #:list-queue-raw
           #:list-queue-enqueue
           #:list-queue-dequeue))

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
