(defpackage #:gtwiwtg.examples
  (:use #:cl #:gtwiwtg)
  (:export #:perms #:all-primes))

(in-package :gtwiwtg.examples)

;; permutations


(defun fill-and-insert (idx elem vec buffer)
  "A utilty function that modifies BUFFER. 

The length of BUFFER is one greater than the length of VEC. 

This function fills the first IDX fields of BUFFER with the first IDX
fields of VEC. Fills the field of BUFFER at IDX with ELEM.  Fills the
remaining fields of BUFFER with the remaining fields of VEC.
"
  (loop :for i :below (length buffer)
     :when (= i idx) :do (setf (aref buffer idx) elem)
     :when (< i idx) :do (setf (aref buffer i)
                               (aref vec i))
     :when (> i idx) :do (setf (aref buffer i)
                               (aref vec (1- i))))  )

(defun thread-through (elem vec)
  "Creates a generator that produces a series of N vectors of length
N, where N is one greater than the length of VEC.  The vectors
produced by this generator have the same elements of VEC but have ELEM
inserted at each possible spot, N spots in all. 

Note: The generator reuses the memory that it returns on each step. If
you intend to collect to products of the generator, you should copy
them to somehow first.
"
  (let ((buffer (concatenate 'vector vec (list elem)))) ;; reusable buffer
    (map! (lambda (idx)
            (fill-and-insert idx elem vec buffer)
            buffer)
          (range :from 0 :to (length vec) :inclusive t))))


(defun perms (vec)
  "Creates a generator that produces all of the permutations of the
vector VEC, one at a time."
  (if (= 1 (length vec)) (seq (list vec))
      (let ((elem (elt vec 0))
            (subperms (perms (make-array (1- (length vec))
                                         :displaced-to vec
                                         :displaced-index-offset 1
                                         :element-type (array-element-type vec)))))
        (inflate! (lambda (subperm) (thread-through elem subperm)) subperms))))

;; primes

(defun prime-p (n)
  (loop
     :for x :from 2 :upto (sqrt n)
     :when (zerop (mod n x)) :do (return nil)
     :finally (return t)))

(defun all-primes ()
  (filter! #'prime-p (range :from 1)))


