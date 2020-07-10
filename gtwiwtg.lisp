(defpackage #:gtwiwtg (:use #:cl))
(in-package :gtwiwtg)

;;; Generator Protocol ;;;

;; None of the following are meant to be called directly by users of the library.

(defgeneric next (gen)
  (:documentation "Returns the next value of this generator, if
  available. Unspecified behavior if the generator has been exhausted."))

(defgeneric has-next-p (gen)
  (:documentation "Returns true if next can be called on the generator"))

(defgeneric stop (gen)
  (:documentation "Explicitly stops the generator. Specialize :after
  methods to implement any clean-up that needs to be done when the
  generator has been consumed."))

;;; Base Generator Class ;;; 

(defclass generator! ()
  ((dirty-p
    :accessor dirty-p
    :initform nil
    :documentation "Indicates whether or not this generator has
    generated any values yet, or if it should behave as if it has.")
   (stopped-p
    :accessor stopped-p
    :initform nil
    :documentation "Indicates whether or not this generator has been
    explicitly stopped. All consumers explicitly stop the generators
    the consume.")))

(defmethod stop ((g generator!))
  (setf (stopped-p g) t))

(defmethod has-next-p :around ((g generator!))
  (unless (stopped-p g) (call-next-method)))

(defun make-dirty (g) (setf (dirty-p g) t))

;;; Utility Class Builder ;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (symb)
    (read-from-string (format nil ":~a" symb))))

(defmacro a-generator-class (name supers &rest slots)
  `(defclass ,name ,(cons 'generator! supers)
     ,(mapcar (lambda (def)
                 (if (consp def)
                     `(,(car def)
                        :initarg ,(make-keyword (car def))
                        :initform ,(second def))
                     `(,def :initarg ,(make-keyword def)
                            :initform nil)))
               slots)))

;;; Generator Classes ;;; 

(a-generator-class range-backed-generator! ()
         (at 0) to (by 1) inclusive (comparator #'<))

(defmethod has-next-p ((g range-backed-generator!))
  (with-slots (to current comparator by at) g
    (or (not to)
          (funcall comparator
                   (+ by at)
                   to))))

(defmethod next ((g range-backed-generator!))
  (with-slots (at by) g
    (incf at by)
    at))

(a-generator-class sequence-backed-generator! ()
         sequence index)

(defmethod has-next-p ((g sequence-backed-generator!))
  (with-slots (index sequence) g
    (< index (1- (length sequence)))))

(defmethod next ((g sequence-backed-generator!))
  (with-slots (index sequence) g
    (incf index)
    (elt sequence index)))

(a-generator-class list-backed-generator! ()
         list)

(defmethod has-next-p ((g list-backed-generator!))
  (consp (slot-value g 'list)))

(defmethod next ((g list-backed-generator!))
  (pop (slot-value g 'list)))

(a-generator-class thunk-backed-generator! ()
         next-p-fn
         next-fn
         stop-fn)

(defmethod has-next-p ((g thunk-backed-generator!))
  (funcall (slot-value g 'next-p-fn) ))

(defmethod next ((g thunk-backed-generator!))
  (funcall (slot-value g 'next-fn)))

(defmethod stop :after ((g thunk-backed-generator!))
  (with-slots (stop-fn) g
    (when stop-fn
      (funcall stop-fn))))

(a-generator-class stream-backed-generator! ()
         stream reader)

(defmethod has-next-p ((g stream-backed-generator!))
  (open-stream-p (slot-value g 'stream)))

(defmethod next ((g stream-backed-generator!))
  (with-slots (reader stream) g
    (let ((read-value (funcall reader stream)))
      (unless read-value
        (close stream))
      read-value)))

(defmethod stop :after ((g stream-backed-generator!))
  (close (slot-value g 'stream)))         

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
    (make-instance 'range-backed-generator!
                   :comparator comparator
                   :inclusive inclusive
                   :at (- from by)
                   :to to
                   :by by)))

(defun times (n)
  "Shorthand for (RANGE :TO N)"
  (range :to n))

(defun seq (sequence &key (start 0))
  "Turns a sequecne (a list, vector, string, etc) into a
generator. The resulting generator will generate exactly the memebers
of the sequence."
  (assert (typep sequence 'sequence))
  (if (consp sequence)
      (make-instance 'list-backed-generator!
                     :list (nthcdr start sequence))
      (make-instance 'sequence-backed-generator!
                     :sequence sequence
                     :index (1- start))))

(defun from-thunk-until (thunk &optional (until (constantly nil)) clean-up)
  "Creates a generator that produces a series of value by successively
calling (FUNCALL THUNK).  The iterator stops whenever (FUNCALL UNTIL)
is non null.

If a CLEAN-UP thunk is supplied, it will be run after consumption of
the new generator has finished. I.e. when passing this form to a
consumer such as FOR, FOLD, COLLECT, etc.

By default, UNTIL is the function (CONSTANTLY NIL). I.e. it will
generate forever."
  (assert (and (every #'functionp (list thunk until))
               (or (null clean-up) (functionp clean-up))))
  (make-instance 'thunk-backed-generator! 
                 :stop-fn clean-up
                 :next-p-fn (complement until)
                 :next-fn thunk))


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

> (let ((fibs (from-recurrence #'+ 1 0)))
     (take 10 fibs))

(1 2 3 5 8 13 21 34 55 89)

"
  (let* ((history (cons n-1 n-m))
         (thunk  (lambda ()
                   (let ((nth (apply rec history)))
                     (setf history (cons nth (butlast history)))
                     nth))))
    (from-thunk thunk)))


(defun repeater (&rest args)
  "Produces a generator that produces an infinite series consisting in
the values passed as ARGS looped forever."
  (let ((state (copy-list args)))
    (from-thunk
     (lambda ()
       (unless state
         (setf state (copy-list args)))
       (pop state)))))


(defun noise (&optional (arg 1.0))
  "Creates a generator that produces an infinite series of random
  numbers that are the result of calling (RANDOM ARG)."
  (from-thunk (lambda () (random arg))))


(defun from-input-stream (stream stream-reader)
  "Create a generator from a STREAM.

You must supply as STREAM-READER function that accepts the stream as
its only argument and returns NIL if the stream has run out of input,
Non-NIL otherwise.

A quirk is that the last value returned from this generator is NIL.

Consumers of the new generator will ensure that the stream is properly
closed..

Here is an example:

 (take 2 (from-input-stream
          (open \"hey.txt\")
          (lambda (s) (read-char s nil nil))))

 (#\\h #\\e)

"
  (make-instance 'stream-backed-generator!
                 :stream stream
                 :reader stream-reader))


(defun file-lines (file)
  "Creates a generator that produces the lines of a file.  The stream
to the file is closed when the generator finishes.

FILE is either a path to a file.

Returns NIL on the last iteration.

"
  (from-input-stream (open file)
                     (lambda (stream) (read-line stream nil nil))))

(defun file-chars (file)
  "Creates a generator that produces the characters of a file. The
stream to the file is closed when the generator finishes.

FILE is either a path to a file.

Returns NIL on the last iteration.
"
  (from-input-stream (open file)
                     (lambda (stream) (read-char stream nil nil))))

(defun file-bytes (file)
  "Creates a generator that produces the bytes of a file. The
stream to the file is closed when the generator finishes.

FILE is either a path to a file.

Returns NIL on the last iteration.
"
  (from-input-stream (open file :element-type '(unsigned-byte 8))
                     (lambda (stream) (read-byte stream nil nil))))

;;; Some utilities

(defun make-queue ()
  (cons nil nil))

(defun enqueue (x q) 
  (push x (car q)))

(defun dequeue (q)
  (when (and (car q) (null (cdr q)))
    (setf (cdr q) (reverse (car q))
          (car q) nil))
  (when (cdr q) (pop (cdr q))))
             
(defun queue-empty-p (q)
  (and (null (car q))
       (null (cdr q))))

;;; Some assertion tests

(defun all-different (things)
  (= (length things) (length (remove-duplicates things))))

(defun all-clean (gens)
  (every (complement #'dirty-p) gens))

(defun all-good (gens)
  (and (all-clean gens) (all-different gens)))

(defun sully-when-clean (gens)
  (assert (all-good gens))
  (dolist (g gens) (make-dirty g)))

;;; MODIFIERS and COMBINATORS

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
"
  (let ((all-gens (list* gen gens)))
    (sully-when-clean all-gens)
    (from-thunk-until
     (lambda ()
       (apply map-fn (mapcar #'next all-gens)))
     (lambda ()
       (some (complement #'has-next-p) all-gens)) ; when at least one has no next value
     (lambda ()
       (dolist (g all-gens) (stop g))))))

(defun filter! (pred gen)
  "Produces a generator that filters out members of GEN that are NIL
when applied to PRED.

Error Condition:
 - If GEN has been used elsewhere, an error will be signalled.
"
  (sully-when-clean (list gen))
  (let (on-deck)
    (from-thunk-until
     (lambda () on-deck) ; consumers always call has-next-p before next
     (lambda ()
       (loop
          :while (has-next-p gen)
          :for candidate = (next gen)
          :when (funcall pred candidate)
          :do (progn
                (setf on-deck candidate)
                (return nil))   ; Don't stop generating, we found one
          :finally (return t))) ; Stop generating, we can't find one.
     (lambda ()
       (stop gen)))))


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
"
  (sully-when-clean (list gen))
  (if (not (has-next-p gen)) gen 
      (let ((sub-gen (funcall fn (next gen))))
        (from-thunk-until
         (lambda () (next sub-gen))
         
         (lambda ()
           (loop
              :until (has-next-p sub-gen)
              :while (has-next-p gen)
              :do
                (stop sub-gen)
                (setf sub-gen (funcall fn (next gen))))
           
           ;; the 'until' thunk must return t when we should stop generating
           ;; hence:
           (not (or (has-next-p sub-gen)
                    (has-next-p gen))))
         
         (lambda ()
           (stop gen)
           (when sub-gen (stop sub-gen)))))))


(defun concat! (gen &rest gens)
  "Returns a generator that is the concatenation of the generators
passed as arguments.

Error Conditions:
 - If any of the generators compare EQL, an error will be signalled.
 - If any of the generators has been used elsewhere, an error will be sigalled.
"
  (sully-when-clean (list* gen gens))
  (inflate! #'identity (seq (list* gen gens))))

(defun zip! (gen &rest gens)
  "Is a shortcut for (MAP! #'LIST GEN1 GEN2 ...)"
  (apply #'map! #'list gen gens))

(defun indexed! (gen)
  "Is shorthand for (ZIP! (RANGE) GEN)"
  (zip! (range) gen))


(defun merge! (comparator gen1 gen2 &rest gens)
  "Emulates the behavior of MERGE (in the ANSI standard), but for generators.

The emulation is not perfect, but it holds in the following sense: If
all the inputs are sorted according to COMPARATOR then the output will
also be sorted according to COMPARATOR.

The generator created through a merge has a length that is the sum of
the lengths of the arguments to MERGE!. Hence, if any of the arguments
is an infinite generator, then the new generator is also infinite.

An example:

> (collect (merge! #'< 
                  (times 4) 
                  (range :from 4 :to 10 :by 2)
                  (range :from -10 :to 28 :by 6)))

 (-10 -4 0 1 2 2 3 4 6 8 8 14 20 26)

Error Conditions:
 - If any of the generators compare EQL, an error will be signalled.
 - If any of the generators have been used elsewhere, an error will be signalled.
"
  (let ((all-gens (list* gen1 gen2 gens)))
    (sully-when-clean all-gens)
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
       (null all-gens))

     (lambda ()
       (dolist (g all-gens) (stop g))))))


(defun skip! (n gen)
  "EXPERIMENTAL. Might be removed.

Returns the generator GEN but without the first N elements. 

Caveats:

 - It is possible that SKIP! will skip the whole source generator,
   returning an empty gernator.
"
  (dotimes (x n) (when (has-next-p gen) (next gen)))
  (setf (slot-value gen 'dirty-p) nil)
  gen)

(defun skip-while! (pred gen)
  "EXPERIMENTAL. Might be removed.

Returns the generator GEN but without the first N elements for
which PRED is non-nil. 

Caveat:
 - It is possible that, for finite generators, skip-while! will skip
   the whole generated sequence. 
 - If the generator is infinite, then skip-while! MIGHT never return.
"
  (loop :while (and (has-next-p gen)
                    (funcall pred (next gen))))
  (setf (slot-value gen 'dirty-p) nil)
  gen)


(defun nfurcate! (count source-generator)
  "EXERIMENTAL. MAY BE REMOVED

Return a list of COUNT copies of GEN.

Caveat: 

 - The generators produced by NFURCATE! allocate new memory as they
   are consumed. This allocation can be O(COUNT * SIZE) in space
   complexity, where SIZE is the size of GEN. Hence, use with caution
   on infinite generators.  

 - The SOURCE-GENERATOR will be cleaned up only after all of the
   returned generators have been consumed. That is if you make some
   generators using NFURCATE! but do use them, then the
   SOURCE-GENERATOR may never be cleaned up. I.e. if it is backed by a
   stream, that stream will not be closed.

"
  (make-dirty source-generator)
  (let ((qs (loop :for _ :below count :collect (make-queue)))
        (stop-cells (loop :for _ :below count :collect (list nil))))
    (loop
       :for build-q :in qs
       :for stop-cell :in stop-cells  
       :collect
         (let ((local-q build-q)
               (local-stop stop-cell))

           (from-thunk-until
            (lambda ()
              (cond ((not (queue-empty-p local-q))
                     (dequeue local-q))

                    ((has-next-p source-generator)
                     (let ((next-v (next source-generator)))
                       (loop :for q :in qs :do (enqueue next-v q))
                       (dequeue local-q)))

                    (t (error "Attempted to get next from a spent generator."))))

            (lambda ()
              (and (not (has-next-p source-generator))
                   (queue-empty-p local-q)))

            (lambda ()
              (setf (car local-stop) t)
              (when (every #'car stop-cells)
                (stop source-generator))))))))

(defun partition! (pred gen)
  "EXPERIMENTAL. MAY BE REMOVED. 

Return a list of two generators that looks like (PASSING FAILING)

PASSING is a generator that produces the values of GEN that pass the
predicate PRED 

FAILING is a generator that produces the values of GEN that do not
pass the predicate PRED

Caveat: 

 - The generators produced by PARTITION! allocate new memory as you
   consume them. This allocation may be O(2 * SIZE) in space
   complexity, where SIZE is the size of GEN. Hence, use with caution
   on infinite generators.

- The generator GEN will be cleaned up only after both returned
  generators have been. Hence, you must consume both or you risk
  leaving some clean up undone. E.g. If GEN is a stream backed
  generator, the stream will not be closed until both PASSING and
  FAILING have been consumed.

"
  (destructuring-bind (gen1 gen2) (nfurcate! 2 gen)
    (list (filter! pred gen1)
          (filter! (complement pred) gen2))))

;;; CONSUMERS

(defmacro for (var-exp gen &body body)
  "The basic generator consumer.

VAR-EXP can be either a symbol, or a form suitable for using as the
binding form in a DESTRUCTURING-BIND.

GEN is an expression that should evaluate to a generator.

BODY is a list of any forms you like. These forms will be evaluated
for each value produced by GEN.

FOR akes care of running any clean up that the generator
requires. E.g. If the generator is backed by an open stream, the
stream will be closed. E.g. If the generator was built using
FROM-THUNK-UNTIL, then the CLEAN-UP thunk will be run before FOR
exits.

Every other consumer is built on top of FOR, and hence, every other
consumer will also perform clean up.

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
            ,expr-body)
       (stop ,gen-var))))

(defmacro fold ((acc init-val) (var-exp gen) expr)
  "The accumulating generator consumer.

ACC is a symbol and INIT-VAL is any lisp expression.  ACC is where
intermediate results are accmulated. INIT-VAL is evaluated to
initialize ACC.

VAR-EXP can be either a symbol, or a form suitable for using as the
binding form in DESTRUCTURING-BIND.

GEN is an expression that should evaluate to a generator.

EXPR is a sigle lisp expression the value of which becomes bound to
ACC on each iteration.

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


