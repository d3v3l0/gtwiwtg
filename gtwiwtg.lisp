(defpackage #:gtwiwtg (:use #:cl))
(in-package :gtwiwtg)

(defclass generator! ()
  ((dirty-p
    :accessor dirty-p
    :initform nil)
   (state
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
  (with-slots (state next-fn dirty-p) gen
    (setf dirty-p t)
    (multiple-value-bind (val new-state) (funcall next-fn state)
      (setf state new-state)
      val)))

(defgeneric has-next-p (gen)
  (:documentation "returns true if next can be called on this generator!"))

(defmethod has-next-p ((gen generator!))
  (with-slots (next-p-fn state) gen
    (funcall next-p-fn state)))


(defun make-dirty (g) (setf (dirty-p g) t))

;;; CONSTRUCTORS

(defun range (&key (from 0) to (by 1) inclusive)
  "Create a generator that produces a series of numbers between FROM
and TO with step size of BY.

When INCLUSIVE is non NIL, then TO will be produced by the generator
if it would be the last member of generate series.  

E.g. 

> (collect (range :to 10))
 
 (0 1 2 3 4 5 6 7 8 9) 

> (collect (range :to 10 :inclusive t))

 (0 1 2 3 4 5 6 7 8 9 10)

> (collect (range :to 10 :by 2 :inclusive t))

 (0 2 4 6 8 10)

> (collect (range :to 10 :by 3 :inclusive t))

 (0 3 6 9)

If TO is NIL, then the generator produces an infinite sequence.

"
  (let ((comparator (if (plusp by)
                        (if inclusive #'<= #'<)
                        (if inclusive #'>= #'>))))
    (make-instance 'generator!
                   :state (list (- from by) to)
                   :next-p-fn (lambda (state) (or (not to)
                                                  (funcall comparator
                                                           (+ by  (first state))
                                                           (second state))))
                   :next-fn (lambda (state)
                              (incf (car state) by)
                              (values (car state) state)))))

(defun times (n)
  "Shorthand for (RANGE :TO N)"
  (range :to n))

(defun seq (sequence)
  "Turns a sequecne (a list, vector, string, etc) into a
generator. The resulting generator will generate exactly the memebers
of the sequence."
  (make-instance 'generator!
                 :state 0
                 :next-p-fn (lambda (state)
                              (< state (length sequence)))
                 :next-fn (lambda (state)
                            (let ((val (elt sequence state)))
                              (values val (1+ state))))))

(defun repeater (&rest args)
  "Produces a generator that produces an infinite series consisting in
the values passed as ARGS looped forever."
  (make-instance 'generator!
                 :state (copy-list args)
                 :next-p-fn (constantly t)
                 :next-fn (lambda (state)
                            (if (cdr state)
                                (values (car state) (cdr state))
                                (values (car args) (copy-list (cdr args)))))))


(defun noise (&optional (arg 1.0))
  "Creates a generator that produces an infinite series of random
  numbers that are the result of calling (RANDOM ARG)."
  (make-instance 'generator!
                 :state nil
                 :next-p-fn (constantly t)
                 :next-fn (lambda (state)
                            (declare (ignore state))
                            (values (random arg) nil))))




(defun from-thunk-until (thunk &optional (until (constantly nil)))
  "Creates a generator that produces a series of value by successively
calling (FUNCALL THUNK).  The iterator stops whenever (FUNCALL UNTIL)
is non null.

By default, UNTIL is the function (CONSTANTLY NIL). I.e. it will
generate forever."
  (make-instance 'generator!
                 :state nil
                 :next-p-fn (lambda (ignore) (declare (ignore ignore)) (not (funcall until)))
                 :next-fn (lambda (ignore)
                            (declare (ignore ignore))
                            (values (funcall thunk) nil))))


(defun from-thunk (thunk)
  "Creates a generator that produces an inifinte series of values that
are the return value of (FUNCALL THUNK). 

If you need to create a stopping condition on your thunk-backed
generator, see FROM-THUNK-UNTIL."
  (from-thunk-until thunk))


(defun from-thunk-times (thunk times)
  "Creates a generator that produces its values by calling 
  (FUNCALL THUNK) exactly TIMES times."
  (let ((thunk-proxy (lambda (ignore) (declare (ignore ignore)) (funcall thunk))))
    (map! thunk-proxy (times times))))

(defun from-recurrence (rec n-1 &rest n-m)
  "Creates a generator from a recurrence relation.

REC is a function of K arguments.

The Nth value of the series generated by the new generator is the result of
calling REC on the previoius K results.

N-1 and N-M are used to initialize the recurrence. (1+ (LENGTH N-M))
should be K, the number of arguments acepted by REC.

Example

> (let ((fibs (from-recurrence (lambda (n-1 n-2) (+ n-1 n-2)) 0 1)))
     (take 10 fibs))

(1 1 2 3 5 8 13 21 34 55)

"
  (let* ((history (cons n-1 n-m))
         (thunk  (lambda ()
                   (let ((nth (apply rec history)))
                     (setf history (cons nth (butlast history)))
                     nth))))
    (from-thunk thunk)))

(defun from-input-stream (stream stream-reader)
  "Create a generator from a STREAM.

You must supply as STREAM-READER function that accepts the stream as
its only argument and returns NIL if the stream has run out of input,
Non-NIL otherwise.

This function will close the stream when it reaches the end.

A quirk is that the last value returned from this generator is NIL.

Avoid using with TAKE or PICK-OUT as the file stream will not be closed.

If you need to use TAKE or PICK-OUT or other consumers that will not
consume the whole generator, you should evaluate the whole generator
within an UNWIND-PROTECTing form like WITH-OPEN-FILE.

e.g.

This is fine:

(with-open-file (input \"hey.txt\")
       (take 2 (from-input-stream
                 input
                 (lambda (s) (read-char s nil nil)))))
(#\\h #\\e)

But this isn't:

(take 2 (from-input-stream
          (open \"hey.txt\")
          (lambda (s) (read-char s nil nil))))

(#\\h #\\e)
"
  (make-instance 'generator!
                 :state stream
                 :next-p-fn #'open-stream-p
                 :next-fn (lambda (stream)
                            (let ((val (funcall stream-reader stream)))
                              (if val
                                  (values val stream)
                                  (progn
                                    (close stream)
                                    (values nil stream)))))))


(defun file-lines (file)
  "Creates a generator that produces the lines of a file.  The stream
to the file is closed when the generator finishes.

FILE is either a path to a file, or is an open character input stream
to a file.

Returns NIL on the last iteration.

Avoid using with TAKE or PICK-OUT as the file stream will not be closed.

If you need to use TAKE or PICK-OUT or other consumers that will not
consume the whole generator, you should evaluate the whole generator
within an UNWIND-PROTECTing form such as WITH-OPEN-FILE.

See the documentation for FROM-INPUT-STREAM for an example of the
distinction.
"
  (from-input-stream (if (streamp file) file (open file))
                     (lambda (stream) (read-line stream nil nil))))

(defun file-chars (file)
  "Creates a generator that produces the characters of a file. The
stream to the file is closed when the generator finishes.

FILE is either a path to a file, or is an open character input stream
to a file.

Returns NIL on the last iteration.

Avoid using with TAKE or PICK-OUT as the file stream will not be closed.

If you need to use TAKE or PICK-OUT or other consumers that will not
consume the whole generator, you should evaluate the whole generator
within an UNWIND-PROTECTing form such as WITH-OPEN-FILE.

See the documentation for FROM-INPUT-STREAM for an example of the
distinction.

"
  (from-input-stream (if (streamp file) file (open file))
                     (lambda (stream) (read-char stream nil nil))))

(defun file-bytes (file)
  "Creates a generator that produces the bytes of a file. The
stream to the file is closed when the generator finishes.

FILE is either a path to a file, or is an open byte input stream to a
file.

Returns NIL on the last iteration.

Avoid using with TAKE or PICK-OUT as the file stream will not be closed.

If you need to use TAKE or PICK-OUT or other consumers that will not
consume the whole generator, you should evaluate the whole generator
within an UNWIND-PROTECTing form such as WITH-OPEN-FILE.

See the documentation for FROM-INPUT-STREAM for an example of the
distinction.
"
  (from-input-stream (if (streamp file) file
                         (open file :element-type '(unsigned-byte 8)))
                     (lambda (stream) (read-byte stream nil nil))))

;;; Some utilities

(defun all-different (things)
  (= (length things) (length (remove-duplicates things))))


(defun all-clean (gens)
  (every (complement #'dirty-p) gens))

(defun all-good (gens)
  (and (all-clean gens) (all-different gens)))

;;; MODIFIERS and COMBINATORS

(defmethod yield-to! (gen1 gen2)
  "GEN1 passes generation control to GEN2. This control will be return
to GEN1 after GEN2 is done. This function modifies GEN1.

Hence, YIELD-TO! can be used within an iteration to conditionally dive
off into some new iteration, knowing that business as usuall will
resume when the \"sub iteration\" finishes.

It is kind of dark magic, and so I don't recommend using it except in
the rareest of circumstances."
  (assert (not (eq gen1 gen2)))
  (make-dirty gen2)
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
  "Maps a function over a number of generators, returning a generator
that produces values that result from calling MAP-FN on those
generators' elements, in sequence.

The resulting generator will stop producing values as soon as any one
of the source generators runs out of arguments to pass to
MAP-FN. I.e. The mapped generator is as long as the shortest argument
generators.

Error Conditions:
 - If any of the generators compare EQL an error will be signalled
 - If any of the generators have been used elsewhere, an error will be signalled.

Caveat: 
 - This function modifies and returns its first generator argument.
"
  (assert (all-good (list* gen gens)))
  (dolist (g gens) (make-dirty g)) ;; to ensure gens wont be re-used after use here.

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
  "Produces a generator that filters out members of GEN that are NIL
when applied to PRED.

Error Condition:
 - If GEN has been used elsewhere, an error will be signalled.

Caveat:
 - This function modifies and returns its generator argument.

"
  (assert (not (dirty-p gen)))
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


(defun inflate! (fn gen)
  "FN is expected to be a function that accepts elements of GEN and
returns a new generator.  

The generator (INFLATE! FN GEN) generates each element of an
intermediate generator (FN X) for each X generated by GEN.

Here is an example:

> (let ((keys (seq '(:name :occupation :hobbies)))
        (vals (seq '(\"buckaroo banzai\" 
                     \"rocker\" 
                     (\"neuroscience\" \"particle physics\" \"piloting fighter jets\")))))
     (collect (inflate! #'seq (zip! keys vals))))

 (:NAME \"buckaroo banzai\" 
  :OCCUPATION \"rocker\" 
  :HOBBIES (\"neuroscience\" \"particle physics\" \"piloting fighter jets\"))

Error Conditions:
 - If GEN has been used elsewhere, an error will be signalled.

Caveat: 
 - INFLATE! Modifies and returns its generator argument.
"
  (assert (not (dirty-p gen)))
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
  "Returns a generator that is the concatenation of the generators
passed as arguments.

Each of the arguments to CONCAT! must be different. If any compare
EQL, an error will be signalled.

Error Conditions:
 - If any of the generators compare EQL, an error will be signalled.
 - If any of the generators has been used elsewhere, an error will be sigalled.

Caveat: 
 - CONCAT! Modifies and returns its first argument.
"
  (assert (all-good (list* gen gens)))
  (dolist (g gens) (make-dirty g)) ;; to help ensure that gens can be combined elsewhere
  (inflate! #'identity (seq (list* gen gens))))

(defun zip! (gen &rest gens)
  "Is a shortcut for (MAP! #'LIST GEN1 GEN2 ...)"
  (apply #'map! #'list gen gens))


(defun merge! (comparator gen1 gen2 &rest gens)
  "Emulates the behavior of MERGE (in the ANSI standard), but for generators.

The emulation is not perfect, but it holds in the following sense: If
all the inputs are sorted according to COMPARATOR then the output will
also be sorted according to COMPARATOR.

The generator created through a merge has a length that is maximal
among the lengths of the arguments to MERGE!. Hence, if any of the
arguments is an infinite generator, then the new generator is also
infinite.

An example:

> (collect (merge! #'< 
                  (times 4) 
                  (range :from 4 :to 10 :by 2)
                  (range :from -10 :to 28 :by 6)))

 (-10 -4 0 1 2 2 3 4 6 8 8 14 20 26)

Error Conditions:
 - If any of the generators compare EQL, an error will be signalled.
 - If any of the generators have been used elsewhere, an errror will be signalled.
"
  (let ((all-gens (list* gen1 gen2 gens)))

    (assert (all-good all-gens))
    (dolist (g all-gens) (make-dirty g))

    (from-thunk-until
     (lambda ()
       (let ((vals (mapcar #'next all-gens)))
         (setq vals (sort vals comparator))

         (setf all-gens
               (delete-if-not #'has-next-p
                              (nconc (when (cdr vals) (list (seq (cdr vals))))
                                     all-gens)))
         (car vals)))
     (lambda ()
       (null all-gens)))))


(defun skip! (n gen)
  "Returns the generator GEN but without the first N elements. 

An error will be signalled if GEN does hot have N ore more elements."
  (dotimes (x n) (next gen))
  (setf (slot-value gen 'dirty-p) nil)
  gen)

(defun skip-while! (pred gen)
  "Returns the generator GEN but without the first N elements for
which PRED is non-nil. 

An error will be signalled if GEN runs out of elements before PRED
returns NIL."
  (loop :while (funcall pred (next gen)))
  (setf (slot-value gen 'dirty-p) nil)
  gen)


;;; CONSUMERS

(defmacro for (var-exp gen &body body)
  "The basic generator consumer.

VAR-EXP can be either a symbol, or a form sutible for using as the
binding form in DESTRUCTURING-BIND.

GEN is an expression that should evaluate to a generator.

BODY is any form you like, it will be evaluated for each value
procuded by GEN.

Example:

(for (x y) (zip! (repeater 'a 'b 'c) (times 5))
  (format t \"~a -- ~a~%\" x y))

A -- 0
B -- 1
A -- 2
B -- 3
A -- 4

"
  (let* ((gen-var (gensym "generator!"))
         (expr-body (if (consp var-exp)
                       `(destructuring-bind ,var-exp (next ,gen-var) ,@body)
                       `(let ((,var-exp (next ,gen-var))) ,@body))))
    `(let ((,gen-var ,gen))
       (loop
          :while (has-next-p ,gen-var)
          :do
            ,expr-body))))

(defmacro fold ((acc init-val) (var-exp gen) expr)
  "The accumulating generator consumer.

ACC is a symbol and INIT-VAL is any lisp expression.  ACC is where
intermediate results are accmulated. INIT-VAL is evaluated to
initialize the value of ACC.

VAR-EXP can be either a symbol, or a form sutible for using as the
binding form in DESTRUCTURING-BIND.

GEN is an expression that should evaluate to a generator.

EXPR is a sigle lisp expression whose value is bound to ACC on each
iteration.

When iteration has concluded, ACC becomes the value of the FOLD form.

Example: standard summing

> (fold (sum 0) (x (times 10)) (+ sum x))

45

Example: a usless calculation

> (fold (acc 0)
        ((x y) (zip! (times 10) (range :by -1)))
        (sqrt (+ acc (* x y))))

 #C(0.444279 8.986663)

Example: building data 

> (fold (plist nil) 
        ((key val)
         (zip! (seq '(:name :occupation :hobbies))
               (seq '(\"buckaroo banzai\" 
                      \"rocker\" 
                      (\"neuroscience\" \"particle physics\" \"piloting fighter jets\")))))
         (cons key (cons val plist)))

 (:HOBBIES (\"neuroscience\" \"particle physics\" \"piloting fighter jets\")
  :OCCUPATION \"rocker\" :NAME \"buckaroo banzai\")

 "
  `(let ((,acc ,init-val))
     (for ,var-exp ,gen
       (setf ,acc ,expr))
     ,acc))


(defun collect (gen)
  "Consumes GEN by collecting its values into a list."
  (nreverse (fold (xs nil) (x gen) (cons x xs))))

(defun take (n gen)
  "Consumes GEN by collecting its first N values into a list"
  (nreverse (fold (xs nil) (x (zip! gen (times n)))
                  (cons (car x) xs))))

(defun pick-out (indexes gen)
  "Consumes GEN by picking out certain members by their index.

INDEXES is a list of non-negative integers.

Returns a list of values from GEN such that each value was an element
of indexes."
  (let ((acc (make-array (length indexes))))
    (for (x idx) (zip! gen (times (1+ (apply #'max indexes))))
      (when (member idx indexes)
        (loop
           :for i :below (length  indexes)
           :for idx2 :in indexes
           :when (= idx2 idx)
           :do (setf (aref acc i) x))))
    (concatenate 'list acc)))

(defun size (gen)
  "Consumes GEN by calculating its size."
  (fold (n 0) (x gen) (1+ n)))

(defun maximum (gen)
  "Consumes GEN, returning its maximum value."
  (fold (m nil) (x gen)
        (if m (max m x) x)))

(defun minimum (gen)
  "Consumes GEN, returning its minimum value."
  (fold (m nil) (x gen)
        (if m (min m x) x)))

(defun average (gen)
  "Consumes GEN, returning its average value."
  (let ((sum 0)
        (count 0))
    (for x gen
      (incf sum x)
      (incf count))
    (/ sum count)))

(defun argmax (fn gen)
  "Consumes GEN. Returns a pair (X . VALUE) such that (FUNCALL FN X)
is maximal among the values of GEN.  VALUE is the value of (FUNCALL FN X)"
  (fold (am nil)
        (arg gen)
        (let ((val (funcall fn arg)))
          (if (or (not am) (> val (cdr am)))
              (cons arg val)
              am))))

(defun argmin (fn gen)
    "Consumes GEN. Returns a pair (X . VALUE) such that (FUNCALL FN X)
is minimal among the values of GEN.  VALUE is the value of (FUNCALL FN X)"
  (fold (am nil)
        (arg gen)
        (let ((val (funcall fn arg)))
          (if (or (not am) (< val (cdr am)))
              (cons arg val)
              am))))


