;;;; Test runner for clef-runner.
;;;;
;;;; Run with: sbcl --noinform --non-interactive --load runner/test/run-tests.lisp
;;;; Or via:   mise run runner:test
;;;;
;;;; A script, not a module: nothing imports it. It still carries a package
;;;; named by its path so the linter can tell it from a file that forgot its
;;;; header, and so its parameters do not land in CL-USER.

(defpackage :clef-runner/test/run-tests
  (:use :cl))

(in-package :clef-runner/test/run-tests)

(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

(defparameter *component-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(defparameter *repo-root*
  (make-pathname :directory (butlast (pathname-directory *component-root*))))

;; Rooted at the repo rather than the component so the sibling clef-conditions
;; fasls land in the same build/ tree, matching every other entry point.
(asdf:initialize-output-translations
 `(:output-translations
   ((,*repo-root* :**/ :*.*.*) (,*repo-root* "build" :**/ :*.*.*))
   :inherit-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *repo-root*))
  (asdf:load-asd (merge-pathnames "clef-runner.asd" *component-root*))
  (asdf:load-system "clef-runner/test/main"))

(sb-ext:exit :code (if (uiop:symbol-call :clef-runner/test/main :run-all-tests)
                       0
                       1))
