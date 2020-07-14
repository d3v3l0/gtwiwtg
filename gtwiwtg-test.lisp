(defpackage :gtwiwtg-test
  (:use :cl :gtwiwtg :prove))

(in-package :gtwiwtg-test)

(is (take 4 (range)) '(0 1 2 3))
(is (collect (range :from 2 :to -1 :by -0.5))
    '(2.0 1.5 1.0 0.5 0.0 -0.5))
(is ())

