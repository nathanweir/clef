;;;; All eighteen ANSI defining forms, plus SETF function names.
;;;;
;;;; The list is complete: these are every DEF* the standard has. See
;;;; docs/surveys/cl-surface-area.md §2. A server that indexes all of these
;;;; indexes every name a conforming program can define at top level.
;;;;
;;;; This file must COMPILE. That is the point -- a fixture that is not valid
;;;; Lisp tests nothing, and the older fixture set had drifted that way.

(defpackage :clef-corpus/defining
  (:use :cl)
  (:nicknames :corpus-def)
  (:export #:make-counter #:counter-value #:with-doubled #:+limit+
           #:shape #:shape-area #:point #:point-x))

(in-package :clef-corpus/defining)

;;; --- the plain five ------------------------------------------------------

(defun make-counter (start)
  "An ordinary function."
  (let ((n start))
    (lambda () (incf n))))

(defmacro with-doubled ((var value) &body body)
  "An ordinary macro, with a destructuring lambda list."
  `(let ((,var (* 2 ,value)))
     ,@body))

(defparameter *tally* 0 "A special variable that is meant to be rebound.")

(defvar *registry* nil "A special variable that is meant to be set once.")

(defconstant +limit+ 100 "A constant.")

;;; --- types and classes ---------------------------------------------------

(deftype small-integer ()
  "A type defined by expansion into another type specifier."
  '(integer 0 100))

(defclass shape ()
  ((name :initarg :name :accessor shape-name :type string)
   (area :initarg :area :accessor shape-area :initform 0))
  (:documentation "A class. Its two :ACCESSOR options define four functions:
SHAPE-NAME, SHAPE-AREA, and the SETF forms of both."))

(defstruct point
  "A structure. Defines MAKE-POINT, POINT-P, COPY-POINT, POINT-X, POINT-Y --
none of which appear literally anywhere in this file."
  (x 0 :type real)
  (y 0 :type real))

(define-condition parse-failure (error)
  ((line :initarg :line :reader parse-failure-line))
  (:report (lambda (c s) (format s "failed at line ~D" (parse-failure-line c))))
  (:documentation "A condition class."))

;;; --- generic functions ---------------------------------------------------

(defgeneric area (shape)
  (:documentation "A generic function. The declaration site users navigate to.")
  ;; A :METHOD option defines a method inline -- a definition nested inside
  ;; another definition, with its own specialized lambda list.
  (:method ((s shape)) (shape-area s))
  (:method ((s point)) 0))

(defmethod area ((s (eql :unit)))
  "An EQL specializer: the method applies to one VALUE, not to a type."
  1)

;;; --- the setf family -----------------------------------------------------

;;; A function whose NAME IS A LIST. Legal, common, and it breaks any indexer
;;; that assumes a definition's name is an atom.
(defun (setf shape-label) (new-value shape)
  (setf (shape-name shape) new-value))

(defmethod (setf area) (new-value (s shape))
  "A method whose name is also a list."
  (setf (shape-area s) new-value))

(defsetf shape-area-alias shape-set-area
  "A short-form DEFSETF: names the setter function for a place.")

(defun shape-area-alias (s) (shape-area s))
(defun shape-set-area (s v) (setf (shape-area s) v))

(define-setf-expander car-of (place)
  "The long form. Returns five values describing how to read and write a place."
  (let ((temp (gensym "TEMP"))
        (store (gensym "STORE")))
    (values (list temp) (list place) (list store)
            `(progn (setf (car ,temp) ,store) ,store)
            `(car ,temp))))

(define-modify-macro doublef () (lambda (x) (* 2 x))
  "Defines a macro that reads, transforms and writes back a place.")

;;; --- the remaining three -------------------------------------------------

(define-symbol-macro *current-limit* +limit+)

(define-compiler-macro make-counter (&whole form start)
  "A compiler macro: an optimisation hook that does not change semantics."
  (if (constantp start) form form))

(define-method-combination sum-combination ()
  ((methods *))
  "A user-defined method combination."
  `(+ ,@(mapcar (lambda (m) `(call-method ,m)) methods)))

;;; --- uses, so nothing above is dead --------------------------------------

(defun exercise ()
  "Reference each defined name at least once, so find-references has something
to find and the compiler confirms every definition above is real."
  (let ((c (make-counter 0))
        (s (make-instance 'shape :name "sq" :area 4))
        (p (make-point :x 1 :y 2)))
    (with-doubled (d 21)
      (setf *tally* (+ d (funcall c) (point-x p) +limit+ *current-limit*))
      (push (area s) *registry*)
      (setf (shape-label s) "renamed")
      (setf (area s) 9)
      (doublef *tally*)
      (handler-case (error 'parse-failure :line 3)
        (parse-failure (e) (parse-failure-line e)))
      (the small-integer 5))))
