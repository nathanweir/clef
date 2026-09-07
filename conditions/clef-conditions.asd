;;; clef-conditions.asd
;;;
;;; This file is the whole manifest, permanently. It never lists source files
;;; and never lists dependencies -- both are derived from the DEFPACKAGE form
;;; at the top of each file (ASDF's package-inferred-system). Adding a file or
;;; a dependency means writing the import where you use it, nothing here.
;;;
;;; The public package name stays `clef-conditions': src/main.lisp is the
;;; entry module, named by its path as the convention requires, and carries
;;; the primary system's name as a nickname. Consumers keep writing
;;; CLEF-CONDITIONS:EXTRACT; inference maps that name to this system.
;;;
;;; See docs/golden-path/packages.md for the convention.

(defsystem "clef-conditions"
  :description "Structured extraction and humane rendering of Common Lisp conditions"
  :author "Nathan Weir"
  :license "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("clef-conditions/src/main")
  :in-order-to ((test-op (load-op "clef-conditions/test/main")))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :clef-conditions/test/main :run-all-tests)
               (error "clef-conditions: tests failed"))))
