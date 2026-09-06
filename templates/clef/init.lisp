;;; Project init: hermetic by default.
;;;
;;; Use it either way:
;;;   sbcl --userinit init.lisp        everyday REPL and scripts
;;;   (load "init.lisp")               from an already-running image
;;;
;;; Two things happen here, and the second is the one that matters:
;;;
;;; 1. The ocicl runtime is loaded, so a dependency named in any DEFPACKAGE is
;;;    fetched, digest-pinned into ocicl.csv, and vendored into ocicl/ the
;;;    first time it is loaded.
;;;
;;; 2. The ASDF source registry is set to THIS PROJECT ONLY, ignoring
;;;    inherited configuration. The default registry includes implicit trees
;;;    like ~/common-lisp/, and a stray checkout there silently outbids your
;;;    vendored, pinned copy -- a build that works for reasons that are not in
;;;    the repository. If you want ambient systems in a dev image, opt back in
;;;    knowingly by re-running INITIALIZE-SOURCE-REGISTRY yourself.

;; SBCL bundles ASDF but does not load it until asked, and this file runs
;; before anything else has asked.
(require :asdf)

(let ((runtime (merge-pathnames "ocicl/ocicl-runtime.lisp" (uiop:xdg-data-home))))
  (if (probe-file runtime)
      (load runtime)
      (error "ocicl is not set up on this machine.~%~
              Install ocicl, then run: ocicl setup~%~
              (expected runtime at ~A)" runtime)))

(asdf:initialize-source-registry
 (list :source-registry
       ;; Anchored to this file's own directory, not the cwd, so starting
       ;; from a subdirectory of the project changes nothing.
       (list :directory (uiop:pathname-directory-pathname *load-truename*))
       :ignore-inherited-configuration))
