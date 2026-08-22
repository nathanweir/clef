;;;; Decode textDocument/semanticTokens/full and show what it actually says.
;;;;
;;;; Semantic tokens are the answer to the thing tree-sitter structurally
;;;; cannot do for Common Lisp: a grammar sees a symbol in head position, but
;;;; whether that symbol names a function, a macro, a special operator or a
;;;; class depends on the image, not the text. The Zed extension's
;;;; highlights.scm tried to fake it with a hardcoded list of ~900 standard
;;;; function names and ~100 macro names -- frozen, incomplete, and blind to
;;;; anything the project itself defines.
;;;;
;;;; The wire format is deltas: flat 5-tuples of
;;;;   (deltaLine, deltaStartChar, length, tokenType, tokenModifiers)
;;;; each relative to the previous token, with tokenType an index into the
;;;; legend and tokenModifiers a bitset. Nothing about that is readable, which
;;;; is exactly why defects here went unnoticed -- decoding it is the only way
;;;; to see what a client will see.

#-quicklisp
(let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file init) (load init)))
(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)
(defparameter *repo-root* (truename "."))
(defparameter *lsp-root* (merge-pathnames "lsp/" *repo-root*))
(require 'sb-posix) (require 'sb-introspect)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *repo-root*))
  (asdf:load-asd (merge-pathnames "clef-lsp.asd" *lsp-root*))
  (asdf:load-system :clef-lsp))
(ql:quickload '(:serapeum :bordeaux-threads :com.inuoe.jzon :babel :cl-ppcre) :silent t)
(setf clef-log:*log-mode* :none)
(handler-bind ((warning #'muffle-warning))
  (dolist (f '("test/package.lisp" "test/framework.lisp"))
    (load (merge-pathnames f *lsp-root*))))

(in-package :clef-test)

;;; Deliberately mixes categories a grammar cannot tell apart: a standard
;;; function, a standard macro, a special operator, a project function, a
;;; project macro, a class, a lambda parameter and a let binding.
(defparameter *code* "(defpackage :tokens-demo (:use :cl))
(in-package :tokens-demo)

(defclass widget ()
  ((size :initarg :size :accessor widget-size)))

(defmacro twice (form) `(progn ,form ,form))

(defun area (w scale)
  (let ((base (widget-size w)))
    (when (plusp base)
      (twice (list base scale)))))
")

(defun modifier-names (bits legend)
  (loop for i from 0 below (length legend)
        when (logbitp i bits) collect (aref legend i)))

(with-direct-handler-test
  (init-server)
  (let* ((path (write-temp-file *code*))
         (uri (format nil "file://~A" path))
         (lines (coerce (uiop:split-string *code* :separator '(#\Newline)) 'vector))
         (types clef-lsp/types/basic:*semantic-token-types*)
         (mods clef-lsp/types/basic:*semantic-token-modifiers*))
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                             "version" 1 "text" *code*))
                  :id nil)
    (let* ((result (response-result-safe
                    (call-handler "textDocument/semanticTokens/full"
                                  (dict "textDocument" (dict "uri" uri)))))
           (data (and (hash-table-p result) (gethash "data" result))))
      (format t "~&legend types:     ~{~A~^ ~}~%" (coerce types 'list))
      (format t "~&legend modifiers: ~{~A~^ ~}~%~%" (coerce mods 'list))
      (cond
        ((null data) (format t "~&NO TOKENS RETURNED~%"))
        (t
         (format t "~&~D token(s)~%~%" (/ (length data) 5))
         (format t "~&~4A ~4A ~24A ~14A ~A~%" "line" "col" "text" "type" "modifiers")
         (format t "~&~A~%" (make-string 72 :initial-element #\-))
         (let ((line 0) (col 0))
           (loop for i from 0 below (length data) by 5
                 do (let ((dline (aref data i))
                          (dcol (aref data (+ i 1)))
                          (len (aref data (+ i 2)))
                          (type (aref data (+ i 3)))
                          (modbits (aref data (+ i 4))))
                      (incf line dline)
                      (setf col (if (zerop dline) (+ col dcol) dcol))
                      (let* ((text (let ((l (aref lines line)))
                                     (subseq l (min col (length l))
                                             (min (+ col len) (length l))))))
                        (format t "~&~4D ~4D ~24A ~14A ~{~A~^,~}~%"
                                line col text
                                (if (< type (length types)) (aref types type) "?")
                                (modifier-names modbits mods)))))))))
    (delete-temp-file path)))
