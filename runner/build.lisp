;;;; Build the standalone clef-run executable.
;;;;
;;;; Run with: sbcl --noinform --non-interactive --load runner/build.lisp
;;;; Or via:   mise run runner:build
;;;;
;;;; Much simpler than lsp/build.lisp, which has to pin shared objects because
;;;; cl-tree-sitter dlopens a C library. This component depends on nothing
;;;; outside SBCL and clef-conditions, so dumping is the whole job.

(require :asdf)

(defparameter *here*
  (make-pathname :directory (pathname-directory *load-truename*)))

(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

(defparameter *source-root*
  (let ((parent (make-pathname :directory (butlast (pathname-directory *here*)))))
    (if (probe-file (merge-pathnames "conditions/clef-conditions.asd" parent))
        parent
        *here*)))

;; Conditional for the same reason as lsp/build.lisp: under a nix builder the
;; source is a read-only store path and nothing can be written beside it.
(let ((build-dir (merge-pathnames "build/" *source-root*)))
  (when (ignore-errors (ensure-directories-exist build-dir) t)
    (asdf:initialize-output-translations
     `(:output-translations
       ((,*source-root* :**/ :*.*.*) (,*source-root* "build" :**/ :*.*.*))
       :inherit-configuration))))

(let ((*standard-output* *error-output*))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *source-root*))
  (asdf:load-asd (merge-pathnames "clef-runner.asd" *here*))
  (asdf:load-system :clef-runner))

(uiop:call-image-dump-hook)

(let ((out (or (uiop:getenv "CLEF_RUN_OUTPUT")
               (merge-pathnames "clef-run" *here*))))
  (format *error-output* "~&Dumping executable to ~A~%" out)
  (sb-ext:save-lisp-and-die
   out
   :executable t
   ;; MAIN returns a code rather than exiting, so that it stays testable in
   ;; process. The toplevel is what turns that into an exit.
   :toplevel (lambda () (sb-ext:exit :code (clef-runner:main)))
   :save-runtime-options t))
