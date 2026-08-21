;;;; Corpus 6: the lexical edges.
;;;;
;;;; Reader conditionals, character literals, escaped symbol names, block
;;;; comments, and multi-byte characters. None of these appeared in any fixture.
;;;;
;;;; The multi-byte case is the one with teeth. Byte-versus-character offsets are
;;;; load-bearing throughout symbols/init.lisp -- CALCULATE-LINE-OFFSETS,
;;;; LINE-CHAR-TO-BYTE-OFFSET, FAST-NODE-TEXT -- and no test has ever shown any
;;;; of them a character wider than one byte. If offsets are confused, every
;;;; position after the first non-ASCII character in a file is wrong.

(defpackage :corpus-edges
  (:use :cl)
  (:export #:naïve-average #:|weird name| #:classify-char))

(in-package :corpus-edges)

;;; Multi-byte characters in identifiers, strings, and comments.
;;; The naïve implementation — it does not guard against an empty list.
(defun naïve-average (numbers)
  "Mean of NUMBERS. Note the ï, é and — above and in this docstring."
  (/ (reduce #'+ numbers) (length numbers)))

(defparameter *unicode-table*
  '(("α" . :alpha) ("β" . :beta) ("λ" . :lambda))
  "Keys here are multi-byte; anything after them on the line is offset.")

(defun lookup-greek (name)
  (cdr (assoc name *unicode-table* :test #'string=)))

;;; A symbol whose name needs bars, containing a space and mixed case.
(defun |weird name| (x)
  "Vertical-bar-escaped symbols are legal and occasionally unavoidable."
  (* x 2))

(defun calls-the-weird-one ()
  (|weird name| 21))

;;; Character literals, including ones that look like delimiters.
(defun classify-char (c)
  (case c
    (#\( :open-paren)
    (#\) :close-paren)
    (#\; :semicolon)
    (#\" :double-quote)
    (#\\ :backslash)
    (#\Space :space)
    (#\Newline :newline)
    (#\λ :lambda-char)
    (t :other)))

;;; Strings containing things that look like code. Nothing in here is a
;;; reference, and an indexer that searches text rather than the tree will
;;; think otherwise.
(defparameter *decoys*
  (list "(defun classify-char (c) :not-a-definition)"
        "naïve-average is mentioned but not referenced"
        ";; this looks like a comment but is a string")
  "Every one of these is data.")

#| A block comment.
   (defun inside-block-comment () :also-not-a-definition)
   It spans several lines and contains balanced parens. |#

;;; Reader conditionals. On SBCL the first branch is read and the second is not,
;;; so an indexer sees text the compiler never will.
#+sbcl
(defun implementation-note ()
  "Only read on SBCL.")

#-sbcl
(defun implementation-note ()
  "Never read here, but still present in the text.")

#+(or sbcl ccl)
(defparameter *supported* t)

(defun uses-conditional-code ()
  (list (implementation-note) *supported*))

;;; A form spanning many lines, to check that ranges cover what they claim.
(defun multi-line-form (a
                        b
                        c)
  (+ a
     b
     c))
