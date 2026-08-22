;;;; Every ANSI form that establishes a lexical binding.
;;;;
;;;; See docs/surveys/cl-surface-area.md §3. clef currently handles four of
;;;; these (LET, LET*, FLET/LABELS/MACROLET, lambda lists); the rest are why
;;;; this file exists.
;;;;
;;;; Every binding here deliberately SHADOWS an outer name of the same name, so
;;;; that a server which does not understand the form resolves to the wrong one
;;;; and the sweep can see it. Shadowing is the whole test.

(defpackage :clef-corpus/binding (:use :cl))
(in-package :clef-corpus/binding)

;;; The name every form below shadows. If go-to-definition on an inner USE of
;;; TARGET lands here, the binding form was not understood.
;;;
;;; Deliberately a FUNCTION and not a DEFVAR. A DEFVAR would proclaim TARGET
;;; special *globally and permanently*, and a special variable cannot be bound
;;; by SYMBOL-MACROLET -- which is what WITH-SLOTS and WITH-ACCESSORS expand
;;; into. So `(defvar target)' anywhere in a program makes `(with-slots (target)
;;; ...)' a compile error everywhere else in it. This is real, it is surprising,
;;; and the first draft of this file hit it.
(defun target () :outer-function)

;;; Specials get the earmuff convention precisely so they cannot collide with
;;; lexical names this way. The convention is load-bearing, not decoration.
(defvar *outer* :outer)

;;; --- the two clef already knows ------------------------------------------

(defun uses-let (target)
  (let ((target (list target)))          ; init form's TARGET is the parameter
    target))                             ; body's TARGET is the binding

(defun uses-let* (target)
  (let* ((target (list target))          ; not visible in its own init form
         (other (cons target nil)))      ; visible here
    (list target other)))

;;; --- multiple values and destructuring -----------------------------------

(defun uses-multiple-value-bind (input)
  (multiple-value-bind (target extra) (floor input 2)
    (list target extra)))

(defun uses-destructuring-bind (spec)
  ;; A lambda list that is a TREE, with nested sublists, defaults and keywords.
  ;; Order is fixed by the standard: required, &optional, &rest, &key, &aux.
  (destructuring-bind (target (nested-a nested-b) &optional (opt target)
                       &rest more &key (kw 0))
      spec
    (list target nested-a nested-b opt kw more)))

;;; --- the DO family -------------------------------------------------------

(defun uses-do ()
  ;; DO binds like LET: the step forms see the OLD values.
  (do ((target 0 (1+ target))
       (acc '() (cons target acc)))
      ((> target 3) acc)))

(defun uses-do* ()
  ;; DO* binds like LET*: later bindings see earlier ones immediately.
  (do* ((target 0 (1+ target))
        (doubled (* 2 target) (* 2 target)))
       ((> target 3) doubled)))

(defun uses-dolist (items)
  (dolist (target items :done)
    (print target)))

(defun uses-dotimes (n)
  (dotimes (target n :done)
    (print target)))

(defun uses-do-symbols ()
  (let ((acc '()))
    (do-external-symbols (target :cl acc)
      (push target acc))))

;;; --- LOOP, a sublanguage of its own --------------------------------------

(defun uses-loop (items)
  ;; FOR, WITH, INTO, ACROSS, BEING are LOOP SYNTAX, not references to
  ;; anything. TARGET, ACC and CH are real bindings.
  (loop with acc = 0
        for target in items
        for i from 0
        do (incf acc target)
        collect target into collected
        finally (return (list acc collected))))

(defun uses-loop-across (vec)
  (loop for target across vec
        when (plusp target) sum target))

(defun uses-loop-hash (table)
  (loop for target being the hash-keys of table
        using (hash-value v)
        collect (cons target v)))

;;; --- PROG: LET + TAGBODY + BLOCK NIL, all at once ------------------------

(defun uses-prog (n)
  (prog ((target 0))
     (setf target n)
     (when (zerop target) (go done))
     (decf target)
   done
     (return target)))

(defun uses-prog* (n)
  (prog* ((target n)
          (doubled (* 2 target)))
     (return doubled)))

;;; --- symbol macros -------------------------------------------------------

(defun uses-symbol-macrolet (cell)
  ;; TARGET here is not a variable at all: every reference EXPANDS to (car cell).
  (symbol-macrolet ((target (car cell)))
    (setf target 1)
    target))

;;; --- the WITH- family ----------------------------------------------------

(defclass holder () ((slot-a :initform 1 :accessor holder-a)))

(defun uses-with-slots (obj)
  (with-slots ((target slot-a)) obj
    target))

(defun uses-with-accessors (obj)
  (with-accessors ((target holder-a)) obj
    target))

(defun uses-with-open-file (path)
  (with-open-file (target path :if-does-not-exist nil)
    (when target (read-line target nil))))

(defun uses-with-output-to-string ()
  (with-output-to-string (target)
    (format target "written")))

(defun uses-with-input-from-string (text)
  (with-input-from-string (target text)
    (read-line target nil)))

(defun uses-with-hash-table-iterator (table)
  ;; Binds TARGET as a LOCAL MACRO, not as a variable.
  (with-hash-table-iterator (target table)
    (multiple-value-bind (more k v) (target)
      (list more k v))))

;;; --- conditions bind too --------------------------------------------------

(defun uses-handler-case (thunk)
  (handler-case (funcall thunk)
    ;; Each clause binds the condition object.
    (error (target) (princ-to-string target))
    (:no-error (target) target)))

(defun uses-restart-case (thunk)
  (restart-case (funcall thunk)
    (use-value (target) target)
    (retry () :retried)))

(defun uses-handler-bind (thunk)
  (handler-bind ((error (lambda (target) (print target))))
    (funcall thunk)))

;;; --- local functions ------------------------------------------------------

(defun uses-flet (n)
  (flet ((target (x) (* 2 x)))          ; body of TARGET cannot see TARGET
    (target n)))

(defun uses-labels (n)
  (labels ((target (x) (if (zerop x) 0 (helper (1- x))))
           (helper (x) (target x)))     ; mutually visible, both directions
    (target n)))

(defun uses-macrolet (n)
  (macrolet ((target (x) `(* 2 ,x)))
    (target n)))

;;; --- lambda lists in their full glory -------------------------------------

(defun uses-full-lambda-list (target &optional (opt target) (opt2 opt opt2-p)
                              &rest rest
                              &key (kw target) ((:other alias) 0) &aux (a (list target)))
  ;; Defaults see EARLIER parameters -- a lambda list binds like LET*.
  (list target opt opt2 opt2-p rest kw alias a))

(defun uses-lambda ()
  (mapcar (lambda (target) (* target target)) '(1 2 3)))
