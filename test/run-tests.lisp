;;; CLEF LSP Test Runner
;;;
;;; Run with: sbcl --script test/run-tests.lisp
;;; Or via: just test

#-quicklisp
(let ((quicklisp-init
        (merge-pathnames "quicklisp/setup.lisp"
                         (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Suppress most output during loading
(setf *compile-verbose* nil)
(setf *compile-print* nil)
(setf *load-verbose* nil)
(setf *load-print* nil)

;; Get project root from script location
(defparameter *project-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(defun project-path (relative-path)
  "Create absolute path from project-relative path"
  (merge-pathnames relative-path *project-root*))

;; Require necessary dependencies
(require 'sb-posix)
(require 'sb-introspect)

;; Clear cached fasl files that might be stale
(asdf:clear-system :clef)

;; Load the main system
(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (project-path "clef.asd"))
  (asdf:load-system :clef))

;; Load test dependencies
(ql:quickload '(:serapeum :bordeaux-threads :com.inuoe.jzon :babel :cl-ppcre) :silent t)

;; Completely suppress logging during tests
(setf clef-log:*log-mode* :file)
(setf clef-log:*log-file-path* #p"/dev/null")

;; Load test files with warnings suppressed
(handler-bind ((warning #'muffle-warning))
  (load (project-path "test/package.lisp"))
  (load (project-path "test/framework.lisp"))
  (load (project-path "test/lifecycle-tests.lisp"))
  (load (project-path "test/document-tests.lisp"))
  (load (project-path "test/diagnostic-tests.lisp")))

;; Run tests
(let ((success (clef-test:run-all-tests)))
  (sb-ext:exit :code (if success 0 1)))
