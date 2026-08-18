;;;; Test runner for clef-conditions.
;;;;
;;;; Run with: sbcl --noinform --non-interactive --load conditions/test/run-tests.lisp
;;;; Or via:   mise run conditions:test

(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

(defparameter *component-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

;; Compile into a component-local build/ directory, matching every other entry
;; point in the repo.
(asdf:initialize-output-translations
 `(:output-translations
   ((,*component-root* :**/ :*.*.*) (,*component-root* "build" :**/ :*.*.*))
   :inherit-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "clef-conditions.asd" *component-root*))
  (asdf:load-system :clef-conditions/test))

(sb-ext:exit :code (if (funcall (find-symbol "RUN-ALL-TESTS" :clef-conditions/test))
                       0
                       1))
