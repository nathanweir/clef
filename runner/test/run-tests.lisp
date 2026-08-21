;;;; Test runner for clef-runner.
;;;;
;;;; Run with: sbcl --noinform --non-interactive --load runner/test/run-tests.lisp
;;;; Or via:   mise run runner:test

(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

(defparameter *component-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(defparameter *repo-root*
  (make-pathname :directory (butlast (pathname-directory *component-root*))))

;; Rooted at the repo rather than the component so the sibling clef-conditions
;; fasls land in the same build/ tree, matching every other entry point.
(asdf:initialize-output-translations
 `(:output-translations
   ((,*repo-root* :**/ :*.*.*) (,*repo-root* "build" :**/ :*.*.*))
   :inherit-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *repo-root*))
  (asdf:load-asd (merge-pathnames "clef-runner.asd" *component-root*))
  (asdf:load-system :clef-runner/test))

(sb-ext:exit :code (if (funcall (find-symbol "RUN-ALL-TESTS" :clef-runner/test))
                       0
                       1))
