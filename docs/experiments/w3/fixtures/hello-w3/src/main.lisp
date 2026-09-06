;;; One file, one package, and the DEFPACKAGE is the manifest:
;;;   - :import-from names an external dependency symbol-by-symbol
;;;   - :local-nicknames names two more without inheriting anything
;;; ASDF's package-inferred-system reads all three clause kinds for
;;; dependencies (verified in asdf.lisp's PACKAGE-DEPENDENCIES), so this file
;;; declares its own build inputs with no :use except :cl.
(defpackage :hello-w3/src/main
  (:use :cl)
  (:import-from :alexandria :when-let)
  (:local-nicknames (:re :cl-ppcre)
                    (:util :hello-w3/src/util))
  (:export :run))

(in-package :hello-w3/src/main)

(defun run ()
  (when-let ((m (re:scan-to-strings "w3-\\S*" "package w3-inferred works")))
    (format t "RUN: ~A / ~A~%" m (util:greeting))))
