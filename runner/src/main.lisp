;;;; The entry module, and the public face of the component.
;;;;
;;;; Re-exports the three implementation packages and carries the primary
;;;; system's name as a nickname, so `clef run', build.lisp and the tests keep
;;;; writing CLEF-RUNNER:MAIN, CLEF-RUNNER:WITH-RUNTIME and so on. Each file
;;;; exports exactly its public part; helpers shared between files but not
;;;; meant for callers are imported by name and never reach this package.

(uiop:define-package :clef-runner/src/main
  (:nicknames :clef-runner)
  (:use-reexport :clef-runner/src/runtime
                 :clef-runner/src/compile
                 :clef-runner/src/cli))

(in-package :clef-runner/src/main)
