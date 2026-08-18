;;;; What structured information do SBCL's compiler conditions actually carry?
;;;;
;;;; Both halves of W0 turn on this, and so does clef's own diagnostic.lisp,
;;;; which currently regex-scrapes the PRINTED message to recover the offending
;;;; symbol and then searches the source text for it. Its own TODO says the
;;;; conditions carry line/column info that should be used instead.
;;;;
;;;; If the structured data is there, one extraction layer serves the condition
;;;; formatter (W0) and the language server's diagnostics (W1) both.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load 01-structured-source-location.lisp

(require :sb-introspect)

;; The probe silences the compiler by rebinding *standard-output*, so hold onto
;; the real one for reporting.
(defparameter *report* *standard-output*)

(defparameter *bad-source* "
(defpackage :probe-pkg (:use :cl))
(in-package :probe-pkg)

(defun calls-undefined ()
  (no-such-function 1 2))

(defun reads-undefined ()
  (+ 1 no-such-variable))

(defun unused-binding ()
  (let ((never-used 5))
    42))

(defun wrong-arity ()
  (calls-undefined 1 2 3))

(defun bad-type ()
  (the string (+ 1 2)))
")

(defun describe-condition (c)
  "Dump everything structurally reachable from a compiler condition."
  (format *report* "~&~%--- ~A ---~%" (type-of c))
  (format *report* "  printed: ~A~%"
          (let ((s (princ-to-string c)))
            (if (> (length s) 100) (concatenate 'string (subseq s 0 100) "...") s)))
  ;; Every slot the condition class defines.
  (let ((class (class-of c)))
    (dolist (slot (sb-mop:class-slots class))
      (let ((name (sb-mop:slot-definition-name slot)))
        (when (slot-boundp c name)
          (let ((v (slot-value c name)))
            (format *report* "    slot ~A = ~A~%"
                    name
                    (let ((s (princ-to-string v)))
                      (if (> (length s) 70)
                          (concatenate 'string (subseq s 0 70) "...")
                          s))))))))
  ;; SBCL's compiler error context, if this condition has one.
  (let ((ctx (ignore-errors (sb-c::find-error-context nil))))
    (if ctx
        (progn
          (format *report* "    >> compiler-error-context present~%")
          (dolist (accessor '(sb-c::compiler-error-context-file-name
                              sb-c::compiler-error-context-file-position
                              sb-c::compiler-error-context-original-source-path
                              sb-c::compiler-error-context-context
                              sb-c::compiler-error-context-original-source))
            (let ((v (ignore-errors (funcall accessor ctx))))
              (when v
                (format *report* "       ~A = ~A~%"
                        (string-downcase
                         (subseq (symbol-name accessor)
                                 (length "COMPILER-ERROR-CONTEXT-")))
                        (let ((s (princ-to-string v)))
                          (if (> (length s) 70)
                              (concatenate 'string (subseq s 0 70) "...")
                              s)))))))
        (format *report* "    >> no compiler-error-context~%"))))

(format t "~&SBCL ~A~%" (lisp-implementation-version))
(format t "~&=== compiling a file with deliberate errors ===~%")

(let ((count 0))
  (uiop:call-with-temporary-file
   (lambda (stream path)
     (write-string *bad-source* stream)
     (force-output stream)
     (close stream)
     (handler-bind ((condition (lambda (c)
                                 (when (typep c '(or warning error))
                                   (incf count)
                                   (describe-condition c)))))
       (let ((*error-output* (make-broadcast-stream))
             (*standard-output* (make-broadcast-stream)))
         (ignore-errors
          (let ((fasl (compile-file path :verbose nil :print nil)))
            (when (and fasl (probe-file fasl)) (delete-file fasl)))))))
   :want-stream-p t :want-pathname-p t :type "lisp" :keep nil)
  (format t "~&~%=== ~D condition(s) seen ===~%" count))

;;; Separately: does a READER error carry position information? Those are the
;;; ones clef reports as a bare "Syntax error" today.
(format t "~&~%=== reader error ===~%")
(handler-case
    (with-input-from-string (s "(defun oops (")
      (read s))
  (condition (c)
    (format t "  type: ~A~%" (type-of c))
    (format t "  printed: ~A~%" c)
    (let ((strm (ignore-errors (stream-error-stream c))))
      (when strm
        (format t "  file-position: ~A~%" (ignore-errors (file-position strm)))))))

(format t "~&~%done~%")
