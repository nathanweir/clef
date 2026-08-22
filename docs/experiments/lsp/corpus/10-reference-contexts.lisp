;;;; What a symbol OCCURRENCE means, by position.
;;;;
;;;; See docs/surveys/cl-surface-area.md §4. Indexing definitions is half the
;;;; job; knowing which occurrences are references to them is the other half.
;;;;
;;;; The single most consequential distinction here is QUOTING. 'FOO is a
;;;; literal symbol -- a piece of data that happens to be spelled like a name.
;;;; It is NOT a use of FOO. A server that counts it as one makes rename
;;;; rewrite data, which silently changes what a program means.

(defpackage :clef-corpus/refs (:use :cl))
(in-package :clef-corpus/refs)

(defun collect (x) (list x))
(defvar *setting* 1)
(deftype small () '(integer 0 9))
(defclass widget () ((size :initarg :size :accessor widget-size)))

;;; --- evaluated positions: these ARE references ---------------------------

(defun real-references (widget)
  (list (collect 1)                      ; operator position -- function
        *setting*                        ; argument position -- variable
        #'collect                        ; function quote     -- function
        (funcall #'collect 2)
        (widget-size widget)             ; accessor call
        (the small 3)                    ; TYPE position
        (make-instance 'widget)))        ; ...but 'WIDGET here is a quoted symbol
                                         ; naming a class -- a real reference to
                                         ; the CLASS, in quoted syntax. The hard
                                         ; case: quoted, yet meaningful.

;;; --- quoted positions: these are DATA, not references --------------------

(defparameter *not-references*
  '(collect *setting* widget-size small)
  "Four symbols in a quoted list. NONE is a reference to anything defined
above. Renaming COLLECT must NOT touch this list -- doing so would change a
data literal, and the program would still compile while meaning something
different. That is the worst class of bug a rename can cause.")

(defparameter *also-not-references*
  '(quote collect)
  "QUOTE written out longhand.")

(defun quoted-in-arguments ()
  "GET and PUT here are data passed to a function, not calls."
  (list (member 'collect '(collect other))
        (assoc 'small '((small . 1)))
        (find-symbol "COLLECT")))

;;; --- backquote: data EXCEPT inside unquotes ------------------------------

(defmacro template (n)
  "In the backquoted form, COLLECT is data; ,N and ,@(...) are evaluated. The
comma is the boundary between the two, and it can nest arbitrarily deep."
  `(let ((collect ,n))
     (list 'collect                       ; data
           collect                        ; reference to the LET binding
           ,(* 2 3)                       ; evaluated at macroexpansion time
           ,@(list 1 2))))                ; spliced

(defun uses-template () (template 5))

;;; --- keywords are never references ---------------------------------------

(defun keywords-are-not-references (w)
  "Every keyword here is self-evaluating. :SIZE is not a reference to the slot
named SIZE, even though they are spelled alike."
  (list (make-instance 'widget :size 1)
        (widget-size w)
        :size
        (getf '(:size 1) :size)))

;;; --- LOOP keywords are syntax, not symbols -------------------------------

(defun loop-keywords-are-syntax (items)
  "FOR, IN, COLLECT, WHEN, INTO, FINALLY, RETURN are LOOP's own syntax. They
are read as symbols but refer to nothing. Indexing them as references pollutes
the symbol table with dozens of phantom entries per loop."
  (loop for item in items
        when (plusp item)
          collect item into kept
        finally (return kept)))

;;; --- declarations name things in yet another way -------------------------

(defun declarations (x y)
  "TYPE, IGNORE, OPTIMIZE, SPEED are declaration identifiers, not references.
FIXNUM and SMALL in the type declaration ARE references to types."
  (declare (type fixnum x) (type small y) (ignore y) (optimize (speed 3)))
  x)

;;; --- slot names are their own little namespace ---------------------------

(defun slot-names (w)
  "SIZE inside SLOT-VALUE is a slot name, reached through a quote, and belongs
to the class -- not to any variable or function named SIZE."
  (list (slot-value w 'size)
        (slot-boundp w 'size)))

;;; --- package-qualified references ----------------------------------------

(defun qualified ()
  "The grammar splits PKG:SYM into two halves. Only the name half is the
symbol; renaming must leave the package prefix alone."
  (list (cl:length '(1 2))
        (cl:car '(1))
        #'cl:list))
