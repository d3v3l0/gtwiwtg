;;;; package.lisp

(defpackage #:gtwiwtg
  (:use #:cl)
  (:export #:range
           #:times
           #:seq
           #:repeater
           #:noise
           #:yield-to!
           #:map!
           #:filter!
           #:inflate!
           #:concat!
           #:zip!
           #:iter
           #:fold
           #:collect
           #:take
           #:pick-out
           #:size
           #:maximum
           #:minimum
           #:average
           #:argmax
           #:argmin))
