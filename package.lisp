;;;; package.lisp

(defpackage #:gtwiwtg
  (:use #:cl)
  (:export #:range
           #:times
           #:seq
           #:repeater
           #:yield-to!
           #:map!
           #:filter!
           #:inflate!
           #:concat!
           #:zip!
           #:iter
           #:fold
           #:collect
           #:size
           #:maximum
           #:minimum
           #:average
           #:argmax
           #:argmin))
