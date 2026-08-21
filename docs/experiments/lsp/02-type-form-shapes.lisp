;;;; What does the grammar make of DEFCLASS, DEFSTRUCT, DEFINE-CONDITION and
;;;; DEFTYPE?
;;;;
;;;; The indexer recognises DEFUN-shaped forms through the grammar's dedicated
;;;; :DEFUN node, and DEFPARAMETER/DEFVAR/DEFCONSTANT by comparing the head
;;;; symbol's text. Adding the type-defining forms needs to know which of those
;;;; two shapes they arrive in, and where the interesting names sit.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/lsp/02-type-form-shapes.lisp

(load (merge-pathnames "lsp/load.lisp" (truename ".")))

(defparameter *source* "(defclass shape (base)
  ((name :initarg :name :accessor shape-name)
   (area :initarg :area :reader shape-area :writer set-shape-area)))

(defstruct point x y)

(defstruct (circle (:constructor make-circle)) radius)

(define-condition shape-error (error)
  ((shape :initarg :shape :accessor shape-error-shape)))

(deftype small-int () '(integer 0 100))
")

(defun kind (node)
  (let ((k (first node)))
    (if (consp k) (second k) k)))

(defun dump (node depth source)
  (format t "~v@T~S~@[  ~S~]~%"
          (* 2 depth) (kind node)
          (when (member (kind node) '(:sym-lit :kwd-lit :kwd-symbol))
            (ignore-errors (clef-parser/parser:node-text node source))))
  (dolist (c (cl-tree-sitter:node-children node))
    (dump c (1+ depth) source)))

(let ((tree (clef-parser/parser:parse-string *source*)))
  (loop for form in (remove-if (lambda (n) (eq (kind n) :comment))
                               (cl-tree-sitter:node-children tree))
        for i from 0
        do (format t "~&~%===== top-level form ~A: ~S =====~%" i (kind form))
           (dump form 0 *source*)))
