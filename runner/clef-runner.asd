;;; clef-runner.asd
;;;
;;; This file is the whole manifest, permanently. It never lists source files
;;; and never lists dependencies -- both are derived from the DEFPACKAGE form
;;; at the top of each file (ASDF's package-inferred-system). Adding a file or
;;; a dependency means writing the import where you use it, nothing here.
;;;
;;; The public package name stays `clef-runner': src/main.lisp is the entry
;;; module, named by its path as the convention requires, and carries the
;;; primary system's name as a nickname. The umbrella binary and build.lisp
;;; keep calling CLEF-RUNNER:MAIN.
;;;
;;; See docs/golden-path/packages.md for the convention.

(defsystem "clef-runner"
  :description "A runner that gives a Common Lisp program process-level guarantees"
  :author "Nathan Weir"
  :license "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("clef-runner/src/main")
  :in-order-to ((test-op (load-op "clef-runner/test/main")))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :clef-runner/test/main :run-all-tests)
               (error "clef-runner: tests failed"))))
