;;; Deliberately wrong: the package name does not match the file's path.
;;; Loaded only by experiment E2a, to see what error the mismatch produces.
(defpackage :hello-w3/src/totally-different-name
  (:use :cl))

(in-package :hello-w3/src/totally-different-name)

(defun never-reached () nil)
