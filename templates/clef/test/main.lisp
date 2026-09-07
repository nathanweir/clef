;;; Tests. All of these reach RUN-TESTS below:
;;;
;;;   clef test                              from a shell
;;;   (asdf:test-op :<%= @ app-name %>)      from a REPL
;;;
;;; `clef test' loads this module (the one the .asd's test-op names) and calls
;;; RUN-TESTS directly, so compiler warnings in a test are reported as they
;;; happen rather than deferred to the end of ASDF's compilation unit.
;;;
;;; RUN-TESTS signals on failure, which is what makes a failing check fail the
;;; run -- exit 1 under clef, a failed test-op in a REPL -- rather than
;;; printing sadly and reporting success.
;;;
;;; Deliberately framework-free: CHECK is fifteen lines and owes nothing to
;;; any dependency. Swap in a real framework later if the project outgrows it;
;;; the test-op wiring in the .asd does not change.
(defpackage :<%= @ app-name %>/test/main
  (:use :cl)
  (:local-nicknames (:main :<%= @ app-name %>/src/main))
  (:export :run-tests))

(in-package :<%= @ app-name %>/test/main)

(defvar *checks* 0)
(defvar *failures* '())

(defun check (label got expected)
  (incf *checks*)
  (if (equal got expected)
      (format t "  ok   ~A~%" label)
      (progn
        (push label *failures*)
        (format t "  FAIL ~A~%    got:      ~S~%    expected: ~S~%"
                label got expected))))

(defun run-tests ()
  (let ((*checks* 0)
        (*failures* '()))
    (format t "~&~A tests~%" "<%= @ app-name %>")
    (check "greet defaults to the world" (main:greet) "Hello, world!")
    (check "greet takes a name" (main:greet "clef") "Hello, clef!")
    (format t "~D check~:P, ~D failure~:P~%" *checks* (length *failures*))
    (when *failures*
      (error "~D test~:P failed: ~{~A~^, ~}" (length *failures*) (reverse *failures*)))
    t))
