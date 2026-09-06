;;; The entry point.
;;;
;;; The DEFPACKAGE below is this file's manifest. Every name it binds has an
;;; explicit provenance you can read right here:
;;;
;;;   :import-from     brings in named symbols from a dependency -- and DECLARES
;;;                    the dependency: ocicl fetches and pins it on first load,
;;;                    ASDF orders the build by it.
;;;   :local-nicknames binds a short prefix to a whole package, external or
;;;                    internal, without inheriting anything. Same declaring
;;;                    power as :import-from.
;;;
;;; Adding a dependency or a new project file is done HERE, in the file that
;;; uses it -- never in the .asd, which stays untouched.
(defpackage :<%= @ app-name %>/src/main
  (:use :cl)
  (:local-nicknames (:util :<%= @ app-name %>/src/util))
  (:export :greet :main))

(in-package :<%= @ app-name %>/src/main)

(defun greet (&optional (whom "world"))
  (format nil "Hello, ~A~A" whom (util:punctuation)))

(defun main ()
  (write-line (greet)))
