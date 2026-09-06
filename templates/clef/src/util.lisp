;;; A second file, mostly to demonstrate the shape: one package per file,
;;; named after its project-relative path, exports listed explicitly. Nothing
;;; anywhere else registers this file -- it loads because src/main.lisp
;;; imports it.
(defpackage :<%= @ app-name %>/src/util
  (:use :cl)
  (:export :punctuation))

(in-package :<%= @ app-name %>/src/util)

(defun punctuation ()
  "!")
