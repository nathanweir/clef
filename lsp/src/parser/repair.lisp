(defpackage :clef-lsp/src/parser/repair
  (:use :cl)
  (:export
   #:repair-source
   #:scan-block-comment-depth
   #:scan-depth
   #:scan-in-line-comment
   #:scan-in-string
   #:scan-source))

(in-package :clef-lsp/src/parser/repair)

;;;; Making a half-typed buffer parseable.
;;;;
;;;; Every position-based feature works by walking the scope tree the indexer
;;;; built, and the indexer builds that tree from a tree-sitter parse. When a
;;;; form is unclosed the parse collapses into an ERROR node, no checker
;;;; recognises it, and the file indexes to nothing but its document scope.
;;;;
;;;; For most requests that is invisible, because you ask for hover or
;;;; go-to-definition on code you have finished writing. Completion is the
;;;; exception: it is *only ever* invoked on an incomplete buffer, so it saw the
;;;; collapsed tree every single time. Measured in
;;;; docs/experiments/lsp/10-completion-scope-chain.lisp -- with the buffer
;;;; balanced the chain is LET -> DEFUN -> DOCUMENT -> WORKSPACE and the LET
;;;; binding is right there; one unclosed paren earlier and the whole chain is
;;;; DOCUMENT -> WORKSPACE with zero names in it.
;;;;
;;;; The repair is to append whatever terminators the text is missing.
;;;;
;;;; **Appending is what makes this safe.** Every byte offset in the original
;;;; text keeps its value, so every node range, every line offset and every
;;;; interval the indexer computes is the same as it would have been. Nothing
;;;; downstream needs to know the text was repaired, and no offset needs
;;;; translating back. Inserting anywhere other than the end would not have that
;;;; property.
;;;;
;;;; This is a lexer, not a parser: it needs to know that the `(' in "(" and in
;;;; #\( are not open parens, which is exactly the set of things a reader has to
;;;; track anyway.

(defconstant +max-repair-depth+ 200
  "Give up past this many unclosed forms.

A pathological or binary file can report thousands of unclosed parens, and
appending thousands of characters to feed the parser helps nobody. Real code is
nowhere near this deep.")

(defstruct (scan-state (:conc-name scan-))
  "What the lexer knows at the end of the text."
  (depth 0 :type fixnum)
  (in-string nil)
  (in-line-comment nil)
  ;; Block comments NEST in Common Lisp -- #| #| |# |# is one comment, not two.
  ;; So this is a count, not a flag.
  (block-comment-depth 0 :type fixnum))

(defun scan-source (source)
  "Lex SOURCE and report what is still open at the end.

Handles the four things that make a paren not a paren: strings, character
literals, line comments and block comments."
  (let ((state (make-scan-state))
        (i 0)
        (n (length source)))
    (flet ((peek (k) (when (< (+ i k) n) (char source (+ i k)))))
      (loop
        while (< i n)
        do (let ((c (char source i)))
             (cond
               ;; --- inside a block comment ---------------------------------
               ((plusp (scan-block-comment-depth state))
                (cond
                  ((and (char= c #\#) (eql (peek 1) #\|))
                   (incf (scan-block-comment-depth state)) (incf i 2))
                  ((and (char= c #\|) (eql (peek 1) #\#))
                   (decf (scan-block-comment-depth state)) (incf i 2))
                  (t (incf i))))

               ;; --- inside a line comment ----------------------------------
               ((scan-in-line-comment state)
                (when (char= c #\Newline)
                  (setf (scan-in-line-comment state) nil))
                (incf i))

               ;; --- inside a string ----------------------------------------
               ((scan-in-string state)
                (cond
                  ;; A backslash escapes the next character, including a quote.
                  ((char= c #\\) (incf i 2))
                  ((char= c #\") (setf (scan-in-string state) nil) (incf i))
                  (t (incf i))))

               ;; --- ordinary code ------------------------------------------
               ;; #\( is a character literal whose name happens to be a paren.
               ;; Skipping two characters after #\ is what stops it counting.
               ((and (char= c #\#) (eql (peek 1) #\\))
                (incf i 3))
               ((and (char= c #\#) (eql (peek 1) #\|))
                (incf (scan-block-comment-depth state)) (incf i 2))
               ((char= c #\;) (setf (scan-in-line-comment state) t) (incf i))
               ((char= c #\") (setf (scan-in-string state) t) (incf i))
               ((or (char= c #\() (char= c #\[)) (incf (scan-depth state)) (incf i))
               ((or (char= c #\)) (char= c #\]))
                ;; Clamp at zero. Too many close parens is a real syntax error
                ;; and diagnostics will say so; going negative here would make
                ;; the repair append opening parens, which helps nothing.
                (when (plusp (scan-depth state)) (decf (scan-depth state)))
                (incf i))
               (t (incf i)))))
      state)))

(defun repair-source (source)
  "SOURCE with whatever terminators it is missing appended.

Returns (values repaired-source repaired-p). When nothing is open, returns
SOURCE itself so the common case allocates nothing."
  (let ((state (scan-source source)))
    (if (and (zerop (scan-depth state))
             (not (scan-in-string state))
             (zerop (scan-block-comment-depth state)))
        (values source nil)
        (let ((tail (with-output-to-string (out)
                      ;; Order matters and is the reverse of how they opened:
                      ;; close the innermost lexical context first. A string
                      ;; opened inside a form must be closed before the form.
                      (when (scan-in-string state) (write-char #\" out))
                      (dotimes (k (min (scan-block-comment-depth state)
                                       +max-repair-depth+))
                        (declare (ignore k))
                        (write-string "|#" out))
                      ;; A trailing line comment swallows anything appended to
                      ;; its line, so break the line before closing forms.
                      (when (scan-in-line-comment state) (write-char #\Newline out))
                      (dotimes (k (min (scan-depth state) +max-repair-depth+))
                        (declare (ignore k))
                        (write-char #\) out)))))
          (values (concatenate 'string source tail) t)))))
