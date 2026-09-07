(defpackage :clef-runner/test/main
  (:use :cl)
  (:import-from :clef-runner/test/harness #:*checks* #:*failures*)
  (:local-nicknames (:runtime-tests :clef-runner/test/runtime-tests)
                    (:cli-tests :clef-runner/test/cli-tests))
  (:export #:run-all-tests))

(in-package :clef-runner/test/main)

;;;; Test entry point. Wired to ASDF's test-op in clef-runner.asd and to
;;;; run-tests.lisp; both treat a NIL return as failure.

(defun run-all-tests ()
  (setf *failures* '() *checks* 0)
  (format t "~&Running clef-runner tests~%~%")
  (runtime-tests:run-runtime-tests)
  (cli-tests:run-cli-tests)
  (format t "~&~%~A checks, ~A failure(s)~%" *checks* (length *failures*))
  (null *failures*))
