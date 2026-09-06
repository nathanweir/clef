;;; Consumer of the mis-named package in oops.lisp. Loaded only by experiment
;;; E2b: the mismatch in oops.lisp is SILENT when that file is loaded alone --
;;; inference maps system names to file paths positionally and never checks
;;; the defpackage inside. The failure surfaces here, one step removed, when
;;; a dependent's defpackage asks for the package that file was supposed to
;;; define.
(defpackage :hello-w3/src/needs-oops
  (:use :cl)
  (:local-nicknames (:o :hello-w3/src/oops)))

(in-package :hello-w3/src/needs-oops)
