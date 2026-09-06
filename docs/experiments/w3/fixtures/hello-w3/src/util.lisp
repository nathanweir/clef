(defpackage :hello-w3/src/util
  (:use :cl)
  (:export :greeting))

(in-package :hello-w3/src/util)

(defun greeting ()
  "hello from util")
