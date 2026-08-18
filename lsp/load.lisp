;;; Load script for CLEF
;;; Configures build output directory and loads the system

(require :asdf)
(require :sb-introspect)
(require :sb-concurrency)

;; Resolve the component root from this file's own location, not from
;; (truename "."). An editor -- or the ~/.local/bin/clef wrapper -- launches the
;; server with the *edited project* as its working directory, which is only
;; incidentally this repository. Deriving the root from the cwd meant the load
;; happened to work when editing clef itself and nowhere else.
(defparameter *clef-lsp-root*
  (make-pathname :directory (pathname-directory *load-truename*)))

;; Quiet the compiler's running commentary. Warnings and errors still come
;; through -- these only govern progress chatter. It matters more here than
;; elsewhere: this file is loaded by the from-source server path, where anything
;; reaching fd 1 corrupts the LSP protocol stream.
(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

;; Compile into a project-local build/ directory rather than
;; ~/.cache/common-lisp/, matching build.lisp and test/run-tests.lisp so every
;; entry point agrees on where fasls land.
(asdf:initialize-output-translations
 `(:output-translations
   ((,*clef-lsp-root* :**/ :*.*.*) (,*clef-lsp-root* "build" :**/ :*.*.*))
   ;; Keep default behavior for everything else (system libraries, quicklisp, etc.)
   :inherit-configuration))

;; Register this directory with ASDF
(asdf:load-asd (merge-pathnames "clef-lsp.asd" *clef-lsp-root*))

;; Load the system with style warnings suppressed (third-party libs)
(handler-bind ((style-warning #'muffle-warning))
  (asdf:load-system :clef-lsp :verbose nil))
