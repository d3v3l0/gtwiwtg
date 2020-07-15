(defpackage :gtwiwtg-test
  (:use :cl :gtwiwtg :prove))

(in-package :gtwiwtg-test)

(defmacro autoplan (&rest test-forms)
  `(progn
     (plan ,(length test-forms))
     ,@test-forms
     (finalize)))

(autoplan 

 (is (take 4 (range)) '(0 1 2 3))


 (is (collect (range :from 2 :to -1 :by -0.5))
     '(2.0 1.5 1.0 0.5 0.0 -0.5))


 (is (collect (range :from 2 :to -1 :by -0.5 :inclusive t))
     '(2.0 1.5 1.0 0.5 0.0 -0.5 -1.0))

 (ok (let ((r (range)))
       (take 1 r)
       (gtwiwtg::stopped-p r)))

 (ok (not (gtwiwtg::stopped-p (range))))

 (is '(4 5 6) (collect (seq '(1 2 3 4 5 6) :start 3)))

 (is (collect (filter! (complement #'alpha-char-p) (seq "1234abcd5e6f")))
     '(#\1 #\2 #\3 #\4 #\5 #\6))

 (is '(1 2 3 5 8 13)
     (take 6 (from-recurrence #'+ 1 0)))


 (let ((s (open (asdf:system-source-file "gtwiwtg-test"))))
   (size (from-input-stream s (lambda (s) (read-line s nil nil))))
   (ok (not (open-stream-p s))))

 )


