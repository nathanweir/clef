;;;; Reader-level syntax: the layer below forms.
;;;;
;;;; See docs/surveys/cl-surface-area.md §5. None of this is a "form" and so
;;;; none of it shows up in a survey of macros and special operators -- but all
;;;; of it is in real source, and several items here will break a parser that
;;;; counts parentheses naively.
;;;;
;;;; This file is deliberately hostile. It compiles.

(defpackage :clef-corpus/reader (:use :cl))
(in-package :clef-corpus/reader)

;;; --- character literals that look like delimiters ------------------------

(defparameter *open* #\(
  "An open paren AS A CHARACTER. A parser counting parens will go one too deep
here and never recover for the rest of the file.")

(defparameter *close* #\)
  "And the matching hazard in the other direction.")

(defparameter *chars* (list #\( #\) #\" #\; #\\ #\# #\| #\Space #\Newline #\Tab)
  "Every delimiter the reader has, as data.")

;;; --- strings containing everything ---------------------------------------

(defparameter *tricky-string* "a string with ( unbalanced parens ( and a ; semicolon
and a newline and an escaped \" quote and a backslash \\"
  "Nothing inside a string is syntax.")

(defparameter *comment-lookalike* ";; this is not a comment, it is a string")

;;; --- block comments, which nest ------------------------------------------

#|
A block comment. It may contain (unbalanced parens and "strings and even
;; line comments.

   #|
   And it NESTS, unlike C. A parser that scans forward for the first closing
   marker rather than tracking depth stops at the END of THIS inner comment,
   and reads the remainder of the outer one as code.

   Note this paragraph cannot spell that closing marker literally: writing it
   in prose really does close the comment. The first draft of this file did
   exactly that, and the reader then treated the outer terminator as the start
   of a |vertical-bar symbol| that swallowed the next twenty lines before dying
   at end-of-file. The file about hostile reader syntax was defeated by its own
   example.
   |#

Still inside the outer comment.
|#

(defparameter *after-block-comment* t)

;;; --- symbols with escaped names ------------------------------------------

(defparameter |a symbol with spaces| 1
  "Vertical bars escape a symbol name wholesale, including spaces and case.")

(defparameter |lower-case-preserved| 2
  "Without bars this would read as LOWER-CASE-PRESERVED; with them the name
keeps its case exactly. So these are two DIFFERENT symbols.")

(defun |another odd name| () :odd)

(defparameter *escaped-single* 'sym\ with\ escaped\ spaces
  "Single-character escapes work too.")

;;; --- uninterned symbols ---------------------------------------------------

(defparameter *uninterned* '#:not-interned
  "#: makes a symbol belonging to no package. Common in DEFPACKAGE export
lists, where it avoids interning names into the defining package.")

;;; --- reader conditionals --------------------------------------------------
;;;
;;; The big one. These decide whether code EXISTS for this build. A server that
;;; ignores them indexes definitions that are not there, or misses ones that
;;; are.

#+sbcl
(defun only-on-sbcl () :sbcl)

#-sbcl
(defun only-off-sbcl () :not-sbcl)

#+(or sbcl ccl)
(defun on-either () :either)

#+(and sbcl (not ccl))
(defun on-sbcl-not-ccl () :precise)

;;; A reader conditional can suppress a single subform rather than a whole
;;; definition, which is harder -- the form is still there, one element shorter.
(defparameter *conditional-element*
  (list 1
        #+sbcl 2
        #-sbcl 3
        4))

;;; --- read-time evaluation -------------------------------------------------

(defparameter *computed-at-read-time* #.(+ 1 2)
  "#. runs arbitrary code while READING. A server must NOT evaluate this --
doing so would execute untrusted code from a file merely opened in an editor.
Treating it as opaque is the only safe stance.")

;;; --- dispatch macros producing non-list data ------------------------------

(defparameter *vector* #(1 2 3))
(defparameter *bits* #*10110)
(defparameter *hex* #xDEADBEEF)
(defparameter *binary* #b1011)
(defparameter *octal* #o755)
(defparameter *radix* #3r120)
(defparameter *ratio* 1/3)
(defparameter *complex* #C(1 2))
(defparameter *pathname* #P"/tmp/example")

;;; --- dotted pairs and improper lists --------------------------------------

(defparameter *dotted* '(a . b))
(defparameter *improper* '(a b . c))

;;; --- quote, quasiquote, and the reader's own abbreviations ----------------

(defparameter *quoted* 'symbol)
(defparameter *function-quoted* #'car)
(defparameter *backquoted* `(a ,(+ 1 1) ,@(list 3 4)))

;;; --- a form spanning many lines, with comments inside it ------------------

(defun multiline-form (a       ; a trailing comment mid-lambda-list
                       b
                       ;; a full-line comment mid-lambda-list
                       c)
  "Comments can appear anywhere whitespace can, including inside forms."
  (+ a          ; after an argument
     b
     c))

;;; --- the very last line of a file is an edge case ------------------------

(defun last-form () :end)
