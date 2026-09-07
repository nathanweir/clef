;;;; Does a second LOAD-SYSTEM reload a package-inferred system's files?
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/w3/03-asdf-reload.lisp
;;;;
;;;; Measured 2026-09-06 with ASDF 3.3.7: yes, every time. Each fresh
;;;; LOAD-SYSTEM re-registers every inferred subsystem, because
;;;; SAME-PACKAGE-INFERRED-SYSTEM-P compares the child component's name to
;;;; "lisp" while SYSDEF-PACKAGE-INFERRED-SYSTEM-SEARCH builds it as
;;;; `(,component-type file-type ...)' -- the symbol, not the variable -- so
;;;; the component is named "file-type" and the check never passes.
;;;; Re-registration discards the load stamps; classic systems that depend on
;;;; the inferred one cascade. Cost: 2 ms for conditions/, 28 ms for the
;;;; 53-file LSP; any DEFPARAMETER in a reloaded file resets. Written up in
;;;; docs/surveys/w3-migration-trial.md.
(require :sb-posix)
(load (merge-pathnames "lsp/load.lisp" (uiop:getcwd)))
(format t "~&ASDF ~A~%" (asdf:asdf-version))

(defmethod asdf:perform :before ((o asdf:load-op) (c asdf:cl-source-file))
  (format t "~&  LOAD ~{~A~^/~}~%" (asdf:component-find-path c)))

(defparameter *render-before* (asdf:find-system "clef-conditions/src/render"))
(defparameter *primary-before* (asdf:find-system "clef-conditions"))

(defun probe-report (label)
  (let ((render (asdf:find-system "clef-conditions/src/render"))
        (primary (asdf:find-system "clef-conditions")))
    (format t "~&~A: primary eq ~S, render eq ~S, kinds ~S~%"
            label (eq primary *primary-before*) (eq render *render-before*)
            (length clef-conditions:*exactly-located-kinds*))))

(probe-report"baseline")

(format t "~&--- step 1: load-system :clef-conditions again~%")
(asdf:load-system :clef-conditions)
(probe-report"after step 1")

(format t "~&--- step 2: load-asd runner~%")
(asdf:load-asd (merge-pathnames "runner/clef-runner.asd" (uiop:getcwd)))
(probe-report"after step 2")

(format t "~&--- step 3: load-system :clef-runner~%")
(asdf:load-system :clef-runner)
(probe-report"after step 3")

(format t "~&--- step 4: load-system :clef-runner again~%")
(asdf:load-system :clef-runner)
(probe-report"after step 4")

;; Detail on the render system's sideway deps vs freshly inferred ones.
(let ((sys (asdf:find-system "clef-conditions/src/render")))
  (format t "~&render sideway deps: ~S~%" (asdf:component-sideway-dependencies sys))
  (format t "~&render pathname: ~S~%" (asdf:component-pathname sys))
  (format t "~&primary pathname: ~S~%" (asdf:component-pathname (asdf:find-system "clef-conditions")))
  (format t "~&render source-file: ~S~%" (asdf:system-source-file sys)))
