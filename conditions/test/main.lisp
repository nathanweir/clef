(defpackage :clef-conditions/test/main
  (:use :cl)
  (:import-from :clef-conditions/test/harness #:*checks* #:*failures*)
  (:local-nicknames (:extract-tests :clef-conditions/test/extract-tests)
                    (:render-tests :clef-conditions/test/render-tests))
  (:export #:run-all-tests))

(in-package :clef-conditions/test/main)

;;;; Test entry point. Wired to ASDF's test-op in clef-conditions.asd and to
;;;; run-tests.lisp; both treat a NIL return as failure.
;;;;
;;;; Before the package migration the extraction tests called the render tests
;;;; by an unqualified forward reference that only resolved because the .asd
;;;; listed the files in the right order. The order now comes from the two
;;;; imports above.

(defun run-all-tests ()
  (setf *failures* '() *checks* 0)
  (format t "~&Running clef-conditions tests~%~%")
  (extract-tests:run-extract-tests)
  (render-tests:run-render-tests)
  (format t "~&~%~A checks, ~A failure(s)~%" *checks* (length *failures*))
  (null *failures*))
