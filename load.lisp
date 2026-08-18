;;; Load script for CLEF
;;; Configures build output directory and loads the system

(require :asdf)
(require :sb-introspect)
(require :sb-concurrency)

;; Configure ASDF to place compiled files in project-local build/ directory
;; instead of ~/.cache/common-lisp/ - makes cleanup safer and project self-contained
(let ((project-root (truename ".")))
  (asdf:initialize-output-translations
   `(:output-translations
     ;; Redirect this project's outputs to build/ subdirectory
     ((,project-root :**/ :*.*.*) (,project-root "build" :**/ :*.*.*))
     ;; Keep default behavior for everything else (system libraries, quicklisp, etc.)
     :inherit-configuration)))

;; Register this directory with ASDF
(asdf:load-asd (merge-pathnames "clef.asd" (truename ".")))

;; Load the system with style warnings suppressed (third-party libs)
(handler-bind ((style-warning #'muffle-warning))
  (asdf:load-system :clef :verbose nil))
