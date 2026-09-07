(defpackage :clef-runner/test/harness
  (:use :cl)
  (:export #:check #:check-true #:*checks* #:*failures* #:temp-source))

(in-package :clef-runner/test/harness)

;;;; The check-counting harness shared by every test file in this component,
;;;; plus the scratch-file helper both suites write fixtures through.

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

(defun temp-source (name text)
  "Write TEXT to a project-local scratch file and return its path.

Project-local, not /tmp: the sandboxed environments this runs in do not have a
writable global temp directory."
  (let ((path (merge-pathnames (format nil "tmp/test/runner-~A.lisp" name)
                               (asdf:system-source-directory :clef-runner))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))
