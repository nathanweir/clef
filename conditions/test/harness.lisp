(defpackage :clef-conditions/test/harness
  (:use :cl)
  (:export #:check #:check-true #:*checks* #:*failures*))

(in-package :clef-conditions/test/harness)

;;;; The check-counting harness shared by every test file in this component.
;;;; Deliberately framework-free: a handful of lines that owe nothing to any
;;;; dependency.

(defvar *failures* '())
(defvar *checks* 0)

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (if (funcall test got expected)
      (format t "  ~C[32m✓~C[0m ~A~%" #\Escape #\Escape label)
      (progn
        (push (format nil "~A: expected ~S, got ~S" label expected got) *failures*)
        (format t "  ~C[31m✗~C[0m ~A: expected ~S, got ~S~%"
                #\Escape #\Escape label expected got))))

(defun check-true (label got)
  (check label (and got t) t))
