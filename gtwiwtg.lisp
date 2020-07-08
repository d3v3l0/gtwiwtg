(defpackage #:gtwiwtg (:use #:cl))
(in-package :gtwiwtg)

(defclass generator! ()
  ((state
    :accessor gen-state
    :initarg :state
    :initform (error "no state"))
   (next-p-fn
    :accessor next-p-fn
    :initarg :next-p-fn
    :initform (error "no next-p"))
   (next-fn
    :accessor next-fn
    :initarg :next-fn
    :initform (error "no next-fn"))))

(defgeneric next (gen)
  (:documentation "gets next if available. Throws an error otherwise."))

(defmethod next ((gen generator!))
  (assert (has-next-p gen))
  (with-slots (state next-fn) gen
    (multiple-value-bind (val new-state) (funcall next-fn state)
      (setf state new-state)
      val)))

(defgeneric has-next-p (gen)
  (:documentation "returns true if next can be called on this generator!"))

(defmethod has-next-p ((gen generator!))
  (with-slots (next-p-fn state) gen
    (funcall next-p-fn state)))

;;; CONSTRUCTORS

(defun range (&key (from 0) to (by 1))
  (make-instance 'generator!
                 :state (list (- from by) to)
                 :next-p-fn (lambda (state) (or (not to)
                                                (apply #'< state)))
                 :next-fn (lambda (state)
                            (incf (car state) by)
                            (values (car state) state))))

(defun times (n)
  (range :to n))

(defun seq (sequence)
  (make-instance 'generator!
                 :state 0
                 :next-p-fn (lambda (state)
                              (< state (length sequence)))
                 :next-fn (lambda (state)
                            (let ((val (elt sequence state)))
                              (values val (1+ state))))))

(defun repeater (&rest args)
  (make-instance 'generator!
                 :state (copy-list args)
                 :next-p-fn (constantly t)
                 :next-fn (lambda (state)
                            (if (cdr state)
                                (values (car state) (cdr state))
                                (values (car args) (copy-list (cdr args)))))))

;;; Some utilities

(defun all-different (things)
  (= (length things) (length (remove-duplicates things))))

;;; MODIFIERS and COMBINATORS

(defmethod yield-to! (gen1 gen2)
  (assert (not (eq gen1 gen2)))
  "Gen1 passes generation control to gen2. This control will be return
  to gen1 after gen2 is done. Returns a new generator!. "
  (let ((orig-pred (next-p-fn gen1))
        (orig-fn (next-fn gen1)))
    (with-slots ((s1 state) (p1 next-p-fn) (f1 next-fn)) gen1
      (with-slots ((s2 state) (p2 next-p-fn) (f2 next-fn)) gen2
        (setf s1 (list s1 s2))
        (setf p1 (lambda (state)
                   (or (funcall p2 (second state))
                       (funcall orig-pred (first state)))))
        (setf f1 (lambda (state)
                   (if (funcall p2 (second state))
                       (multiple-value-bind (val new-s2) (funcall f2 (second state))
                         (values val (list (first state) new-s2)))
                       (multiple-value-bind (val new-s1) (funcall orig-fn (car state))
                         (values val (list new-s1 (second state)))))))))))



(defun map! (map-fn gen &rest gens)
  (assert (all-different (list* gen gens)))
  (let ((orig-fns (mapcar #'next-fn (cons gen gens)))
        (orig-preds (mapcar #'next-p-fn (cons gen gens))))
    (setf (gen-state gen) (mapcar #'gen-state (cons gen gens))
          (next-p-fn gen) (lambda (states)
                            (loop
                               :for state :in states
                               :for pred :in orig-preds
                               :unless (funcall pred state) :do (return nil)
                               :finally (return t)))
          (next-fn gen) (lambda (states)
                          (let ((args)
                                (new-states))
                            (loop
                               :for state :in states
                               :for fn :in orig-fns
                               :do (multiple-value-bind (val new-state) (funcall fn state)
                                     (push val args)
                                     (push new-state new-states)))
                            (values (apply map-fn (reverse args))
                                    (reverse new-states))))))
  gen)

(defun filter! (pred gen)
  (let* ((orig-fn (next-fn gen))
         (orig-p-fn (next-p-fn gen))
         (last-good nil)
         (last-known-state (gen-state gen))
         (new-next-p-fn (lambda (state)
                          (or last-good
                              (loop :while (funcall orig-p-fn state)
                                 :do (multiple-value-bind (val new-state) (funcall orig-fn state)
                                       (if (funcall pred val)
                                           (progn  (setf last-good (list val))
                                                   (setf last-known-state (list new-state))
                                                   (return t))
                                           (setf state new-state)))
                                 :finally (return nil))))))

    (setf (next-p-fn gen) new-next-p-fn)

    (setf (next-fn gen) (lambda (state)
                          (declare (ignore state))
                          (let ((tmp-state (car last-known-state))
                                (tmp-val (car last-good)))
                            (setf last-good nil)
                            (setf last-known-state nil)
                            (values tmp-val tmp-state))))
    gen))


(defun bind! (fn gen)
  (let ((orig-fn (next-fn gen))
        (orig-p (next-p-fn gen))
        (orig-state (gen-state gen)))
    (multiple-value-bind (val state) (funcall orig-fn orig-state)
      (setf orig-state state
            (gen-state gen) (funcall fn val)
            (next-p-fn gen) (lambda (sub)
                              (or (has-next-p sub)
                                  (funcall orig-p orig-state)))
            (next-fn gen) (lambda (sub)
                            (if (has-next-p sub)
                                (values (next sub) sub)
                                (multiple-value-bind (val state) (funcall orig-fn orig-state)
                                  (setf orig-state state)
                                  (let ((new-sub (funcall fn val)))
                                    (values (next new-sub) new-sub))))))))
  gen)


(defun concat! (gen &rest gens)
  (assert (all-different (list* gen gens)))
  (bind! #'identity (seq (list* gen gens))))

(defun zip! (gen &rest gens)
  (apply #'map! #'list gen gens))



;;; CONSUMERS

(defmacro iter ((var-exp gen) &body body)
  (let* ((gen-var (gensym "generator!"))
         (expr-body (if (consp var-exp)
                       `(destructuring-bind ,var-exp (next ,gen-var) ,@body)
                       `(let ((,var-exp (next ,gen-var))) ,@body))))
    `(let ((,gen-var ,gen))
       (loop
          :while (has-next-p ,gen-var)
          :do
            ,expr-body))))

(defmacro fold ((fold-var init-val) (var-exp gen) expr)
  `(let ((,fold-var ,init-val))
     (iter (,var-exp ,gen)
       (setf ,fold-var ,expr))
     ,fold-var))

(defun collect (gen)
  (nreverse (fold (xs nil) (x gen) (cons x xs))))

(defun size (gen)
  (fold (n 0) (x gen) (1+ n)))

(defun maximum (gen)
  (fold (m nil) (x gen)
        (if m (max m x) x)))

(defun minimum (gen)
  (fold (m nil) (x gen)
        (if m (min m x) x)))

(defun average (gen)
  (let ((sum 0)
        (count 0))
    (iter (x gen)
      (incf sum x)
      (incf count))
    (/ sum count)))

(defun argmax (fn gen)
  (fold (am nil)
        (arg gen)
        (let ((val (funcall fn arg)))
          (if (or (not am) (> val (cdr am)))
              (cons arg val)
              am))))

(defun argmin (fn gen)
  (fold (am nil)
        (arg gen)
        (let ((val (funcall fn arg)))
          (if (or (not am) (< val (cdr am)))
              (cons arg val)
              am))))


;;; example

(defun fill-and-insert (idx elem vec buffer)
  "A Utility function that inserts ELEM at IDX into BUFFER. For every
other space in BUFFER, the lements of VEC are inserted in order.

Implicity expects (= (LENGTH BUFFER) (1+ (LENGTH VEC))) 

Not meant for general use. just a utility used by THREAD-THROUGH"
  (loop :for i :below (length buffer)
     :when (= i idx) :do (setf (aref buffer idx) elem)
     :when (< i idx) :do (setf (aref buffer i)
                               (aref vec i))
     :when (> i idx) :do (setf (aref buffer i)
                               (aref vec (1- i))))  )

(defun thread-through (elem vec)
  (let ((buffer (concatenate 'vector vec (list elem)))) ;; reusable buffer
    (map! (lambda (idx)
            (fill-and-insert idx elem vec buffer)
            buffer)
          (range :from 0 :to (length vec)))))


(defun perms (vec)
  (if (= 1 (length vec)) (seq (list vec))
      (let ((elem (elt vec 0))
            (subperms (perms (make-array (1- (length vec))
                                         :displaced-to vec
                                         :displaced-index-offset 1
                                         :element-type (array-element-type vec)))))
        (bind! (lambda (subperm) (thread-through elem subperm)) subperms))))
