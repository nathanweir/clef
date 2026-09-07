;;;; Test runner for clef-conditions.
;;;;
;;;; Run with: sbcl --noinform --non-interactive --load conditions/test/run-tests.lisp
;;;; Or via:   mise run conditions:test
;;;;
;;;; A script, not a module: nothing imports it. It still carries a package
;;;; named by its path so the linter can tell it from a file that forgot its
;;;; header, and so its parameters do not land in CL-USER.

(defpackage :clef-conditions/test/run-tests
  (:use :cl))

(in-package :clef-conditions/test/run-tests)

(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

(defparameter *component-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

;; Compile into a component-local build/ directory, matching every other entry
;; point in the repo.
(asdf:initialize-output-translations
 `(:output-translations
   ((,*component-root* :**/ :*.*.*) (,*component-root* "build" :**/ :*.*.*))
   :inherit-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "clef-conditions.asd" *component-root*))
  (asdf:load-system "clef-conditions/test/main"))

(sb-ext:exit :code (if (uiop:symbol-call :clef-conditions/test/main :run-all-tests)
                       0
                       1))
