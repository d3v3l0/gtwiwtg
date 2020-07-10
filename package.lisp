;;;; package.lisp

(defpackage #:gtwiwtg
  (:use #:cl)
  (:export #:range
           #:times
           #:seq
           #:repeater
           #:noise
           #:from-thunk
           #:from-thunk-until
           #:from-thunk-times
           #:from-recurrence
           #:from-input-stream
           #:file-lines
           #:file-chars
           #:file-bytes
           #:map!
           #:filter!
           #:inflate!
           #:concat!
           #:zip!
           #:merge!
           #:skip!
           #:skip-while!
           #:nfurcate!
           #:partition!
           #:for
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
