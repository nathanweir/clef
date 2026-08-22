;;;; Blocks and tagbody tags: two namespaces clef does not model at all.
;;;;
;;;; See docs/surveys/cl-surface-area.md §3. RETURN-FROM takes a BLOCK NAME,
;;;; not a variable and not a function. Resolving it in either of those
;;;; namespaces is wrong, and RETURN-FROM is everywhere in real Lisp -- clef's
;;;; own source uses it constantly.
;;;;
;;;; The subtle part is that most blocks are IMPLICIT. Every DEFUN establishes
;;;; a block named after the function; every LOOP, DO, DOLIST and DOTIMES
;;;; establishes one named NIL. None of these appear as a BLOCK form.

(defpackage :clef-corpus/blocks (:use :cl))
(in-package :clef-corpus/blocks)

;;; --- explicit blocks ------------------------------------------------------

(defun explicit-block (items)
  (block searching
    (dolist (item items)
      (when (eq item :found)
        (return-from searching item)))   ; names the BLOCK, not a variable
    :not-found))

(defun nested-blocks (items)
  "Two blocks in scope at once. RETURN-FROM picks by name, and the inner one
shadows nothing -- both are reachable."
  (block outer
    (block inner
      (dolist (item items)
        (case item
          (:stop-inner (return-from inner :inner))
          (:stop-outer (return-from outer :outer))))
      :fell-through)))

;;; --- the implicit block every DEFUN has ----------------------------------

(defun implicit-defun-block (x)
  "There is no BLOCK form here, yet IMPLICIT-DEFUN-BLOCK is a block name in
scope. Go-to-definition on it should land on the DEFUN."
  (when (null x)
    (return-from implicit-defun-block :empty))
  x)

(defmethod area ((x integer))
  "Methods get an implicit block named after the generic function too."
  (when (minusp x) (return-from area 0))
  x)

(defgeneric area (x))

;;; --- the implicit BLOCK NIL in iteration ---------------------------------

(defun implicit-nil-block (items)
  "RETURN is (RETURN-FROM NIL ...). The block named NIL is established by
DOLIST -- invisibly."
  (dolist (item items :exhausted)
    (when (eq item :hit) (return item))))

(defun loop-nil-block (items)
  (loop for item in items
        when (eq item :hit) do (return :found)
        finally (return :missing)))

;;; --- LAMBDA has no implicit block ----------------------------------------

(defun lambda-has-no-block (items)
  "A LAMBDA establishes no block, so a RETURN inside one refers to whatever
enclosing BLOCK NIL exists -- here, DOLIST's. This trips people up."
  (dolist (item items :done)
    (funcall (lambda (x) (list x)) item)))

;;; --- tagbody and go ------------------------------------------------------

(defun explicit-tagbody (n)
  "TOP, MIDDLE and DONE are TAGS. They are not variables, not functions, and
not blocks. GO reaches them; nothing else does."
  (let ((acc '()))
    (tagbody
       (when (zerop n) (go done))
     top
       (push n acc)
       (decf n)
       (when (plusp n) (go top))
       (go middle)
     middle
       (push :middle acc)
     done)
    acc))

(defun tags-can-be-integers (n)
  "Tags may be integers as well as symbols, which makes them look like data."
  (tagbody
     (go 1)
   1
     (setf n (1+ n))
     (go 2)
   2)
  n)

;;; --- PROG combines block, tagbody and let --------------------------------

(defun prog-is-three-things (n)
  "PROG establishes a LET, a TAGBODY and a BLOCK NIL simultaneously. ACC is a
variable, LOOP-TOP is a tag, and RETURN exits the block."
  (prog ((acc '()))
   loop-top
     (when (zerop n) (return acc))
     (push n acc)
     (decf n)
     (go loop-top)))

;;; --- catch and throw are DYNAMIC, not lexical ----------------------------

(defun dynamic-exit (thunk)
  "CATCH tags are compared at RUNTIME by value, and the THROW may be in another
function entirely. This is genuinely not resolvable statically -- unlike
BLOCK/RETURN-FROM, which is purely lexical."
  (catch :escape
    (funcall thunk)
    :normal))

(defun throws () (throw :escape :thrown))
