;;;; Corpus 4: macros, including ones that define things.
;;;;
;;;; The hard limit of a tree-sitter-first design. A macro that expands into a
;;;; DEFUN defines a function that appears nowhere in the source text, and no
;;;; amount of parsing will find it. This file makes that boundary explicit
;;;; rather than leaving it to be discovered.

(defpackage :corpus-macros
  (:use :cl)
  (:export #:with-timing #:define-accessor-pair #:once-only-demo))

(in-package :corpus-macros)

(defvar *timings* (make-hash-table :test 'equal)
  "Recorded elapsed times, keyed by label.")

(defmacro with-timing ((label) &body body)
  "&BODY, backquote, unquote-splicing, and a gensym."
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(let ((,start (get-internal-real-time)))
       (let ((,result (progn ,@body)))
         (setf (gethash ,label *timings*)
               (- (get-internal-real-time) ,start))
         ,result))))

(defmacro define-accessor-pair (name &optional (default nil defaultp))
  "A macro that DEFINES FUNCTIONS.

GET-NAME and SET-NAME exist after this expands, and neither string appears in
the source. Nothing that reads the text can index them."
  (let ((getter (intern (format nil "GET-~A" name)))
        (setter (intern (format nil "SET-~A" name)))
        (place (intern (format nil "*~A*" name))))
    `(progn
       (defvar ,place ,(if defaultp default nil))
       (defun ,getter () ,place)
       (defun ,setter (value) (setf ,place value)))))

(define-accessor-pair threshold 10)
(define-accessor-pair label "none")

(defmacro once-only-demo ((&rest names) &body body)
  "&REST in a destructured macro lambda list, plus nested backquote."
  (let ((gensyms (loop :for n :in names :collect (gensym (string n)))))
    `(let (,@(loop :for g :in gensyms :collect `(,g (gensym))))
       `(let (,,@(loop :for g :in gensyms :for n :in names
                       :collect ``(,,g ,,n)))
          ,(let (,@(loop :for n :in names :for g :in gensyms
                         :collect `(,n ,g)))
             ,@body)))))

(defmacro with-keys ((&key (start 0) end) &body body)
  "&KEY with defaults in a macro lambda list."
  `(let ((lower ,start) (upper ,end))
     (declare (ignorable lower upper))
     ,@body))

(defun uses-the-macros ()
  "Calls the macro-generated functions. GET-THRESHOLD is defined, but only
after macroexpansion -- a reference here has no textual definition to find."
  (with-timing ("demo")
    (with-keys (:start 1 :end 5)
      (+ (get-threshold) (length (get-label))))))

(macrolet ((twice (form) `(progn ,form ,form)))
  (defun uses-macrolet ()
    (let ((n 0))
      (twice (incf n))
      n)))

(symbol-macrolet ((current-threshold (get-threshold)))
  (defun uses-symbol-macrolet ()
    (* 2 current-threshold)))
