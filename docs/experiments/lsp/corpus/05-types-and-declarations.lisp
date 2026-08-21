;;;; Corpus 5: types, declarations, and structures with options.
;;;;
;;;; DECLARE and DECLAIM appeared in ZERO fixtures, which matters twice over:
;;;; they are ordinary code the indexer must not trip on, and they are the
;;;; substrate for the typing workstream (motivation §7, roadmap W4). The hover
;;;; handler already tries to surface declared types; nothing tested that a file
;;;; containing declarations even indexes correctly.

(defpackage :corpus-types
  (:use :cl)
  (:export #:small-count #:node #:make-node #:node-value
           #:tree-sum #:clamp))

(in-package :corpus-types)

(deftype small-count ()
  "A non-negative fixnum small enough to index a line."
  '(integer 0 4096))

(deftype maybe (thing)
  "A parameterised type -- DEFTYPE takes a lambda list."
  `(or null ,thing))

(declaim (ftype (function (real real real) real) clamp))
(defun clamp (value lower upper)
  "Global FTYPE proclamation plus local declarations."
  (declare (type real value lower upper)
           (optimize (speed 3) (safety 1)))
  (max lower (min upper value)))

(declaim (inline fast-double))
(defun fast-double (n)
  (declare (type fixnum n))
  (the fixnum (* 2 n)))

;;; DEFSTRUCT with options: a conc-name, a named constructor, a predicate, and
;;; typed slots. The generated names -- MAKE-NODE, NODE-VALUE, NODE-P -- appear
;;; nowhere in the text.

(defstruct (node (:conc-name node-)
                 (:constructor make-node (value &optional left right))
                 (:predicate node-p))
  (value 0 :type integer)
  (left nil :type (or null node))
  (right nil :type (or null node)))

(defstruct (leaf (:include node) (:conc-name leaf-))
  (tag :leaf :type keyword))

(defun tree-sum (tree)
  "Recursive walk with a local declaration."
  (declare (type (or null node) tree))
  (if (null tree)
      0
      (let ((value (node-value tree)))
        (declare (type integer value))
        (+ value
           (tree-sum (node-left tree))
           (tree-sum (node-right tree))))))

(defconstant +max-depth+ 64
  "Deepest tree TREE-SUM will handle without complaint.")

(define-symbol-macro *default-node* (make-node 0))

(defun uses-the-symbol-macro ()
  (node-value *default-node*))
