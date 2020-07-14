;;;; gtwiwtg-test.asd

(asdf:defsystem #:gtwiwtg-test
  :depends-on (:gtwiwtg :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "gtwiwtg-test")))
