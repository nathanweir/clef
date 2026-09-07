(defpackage :clef-runner/test/project-tests
  (:use :cl)
  (:import-from :clef-runner)
  (:import-from :clef-runner/test/harness #:check #:check-true)
  (:export #:run-project-tests))

(in-package :clef-runner/test/project-tests)

;;;; Golden-path projects: finding one, running one, testing one.
;;;;
;;;; The fixtures are hand-written miniatures of what `clef new' emits -- an
;;;; init.lisp, a package-inferred stub, src/main, test/main -- rather than
;;;; the template itself, which lives in the LSP component. Their init.lisp
;;;; skips the ocicl runtime: these tests must pass on a machine without
;;;; ocicl, and nothing here needs a dependency fetched.
;;;;
;;;; Everything runs in this image, so each fixture has its own system name.
;;;; The exit-1 case -- a failing RUN-TESTS -- cannot be observed in-image
;;;; because the runtime's debugger hook would take the process down; here it
;;;; is observed as the error propagating, which is what the hook would catch.

(defparameter *fixture-counter* 0)

(defun fixture-root ()
  "A fresh directory under runner/tmp/test/. Not global /tmp: the sandboxed
environments this runs in do not have a writable one."
  (let ((root (merge-pathnames (format nil "tmp/test/project-~D/" (incf *fixture-counter*))
                               (asdf:system-source-directory :clef-runner))))
    (when (probe-file root)
      (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist root)
    root))

(defun write-file (root rel text)
  (let ((path (merge-pathnames rel root)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

(defparameter *init*
  "(require :asdf)
(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)
(asdf:initialize-source-registry
 (list :source-registry
       (list :directory (uiop:pathname-directory-pathname *load-truename*))
       :inherit-configuration))
")

(defun make-project (name &key (entry-point (format nil "~A/src/main:main" name))
                            (main-body "(format t \"args=~{~A~^,~}~%\" uiop:*command-line-arguments*)")
                            (test-body "(format t \"ok~%\")")
                            extra-main
                            (wire-tests t))
  "Scaffold a miniature golden-path project NAME and return its root."
  (let ((root (merge-pathnames (format nil "~A/" name) (fixture-root))))
    (write-file root "init.lisp" *init*)
    (write-file root (format nil "~A.asd" name)
                (format nil "(asdf:defsystem #:~A
  :class :package-inferred-system
  :depends-on (\"~A/src/main\")~@[
  :entry-point ~S~]~:[~;
  :in-order-to ((asdf:test-op (asdf:load-op \"~A/test/main\")))~])
" name name entry-point wire-tests name))
    (write-file root "src/main.lisp"
                (format nil "(defpackage :~A/src/main (:use :cl) (:export :main))
(in-package :~A/src/main)
~@[~A~%~]
(defun main () ~A)
" name name extra-main main-body))
    (write-file root "test/main.lisp"
                (format nil "(defpackage :~A/test/main (:use :cl) (:export :run-tests))
(in-package :~A/test/main)
(defun run-tests () ~A)
" name name test-body))
    root))

(defmacro capturing ((out err) &body body)
  "Run BODY with stdout and the diagnostic stream captured. Returns
(values body-value stdout-text diagnostics-text)."
  `(let ((,out (make-string-output-stream))
         (,err (make-string-output-stream)))
     (let ((value (let ((*standard-output* ,out)
                        (clef-runner:*diagnostic-stream* ,err))
                    ,@body)))
       (values value (get-output-stream-string ,out) (get-output-stream-string ,err)))))

(defun test-finding ()
  (format t "~&finding the project~%")
  (let ((root (make-project "fixfind")))
    (multiple-value-bind (found name) (clef-runner:find-project root)
      (check-true "found from its root" (and found (uiop:pathname-equal found root)))
      (check "  by the .asd's name" name "fixfind"))
    (multiple-value-bind (found name) (clef-runner:find-project (merge-pathnames "src/" root))
      (check-true "found from a subdirectory" (and found (uiop:pathname-equal found root)))
      (check "  same name" name "fixfind"))
    ;; Two .asd files is not a project: which one names the system?
    (write-file root "other.asd" "")
    (check "two .asd files is no project" (clef-runner:find-project root) nil)
    (delete-file (merge-pathnames "other.asd" root))
    ;; The walk stops at the repository root. runner/tmp/test/ has no
    ;; init.lisp, nor does anything above it up to the repo's .git.
    (check "nothing above the fixture counts"
           (clef-runner:find-project (uiop:pathname-parent-directory-pathname root))
           nil)
    ;; ...and inside a repository, a project past the boundary is invisible.
    (let ((nested (merge-pathnames "inner/deeper/" root)))
      (ensure-directories-exist nested)
      (write-file root "inner/.git" "gitdir: elsewhere")
      (check "a .git between here and the project stops the walk"
             (clef-runner:find-project nested)
             nil))))

(defun test-running ()
  (format t "~&running the project~%")
  (let ((root (make-project "fixrun")))
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:run-project root "fixrun" :argv '("a" "b")))
      (check "a clean project runs and exits 0" code clef-runner:+exit-success+)
      (check-true "  the entry point saw its arguments" (search "args=a,b" out))
      (check "  with nothing on the diagnostic stream" err "")))

  ;; The convention's fallback when the stub is silent: say so, then run.
  (let ((root (make-project "fixnoentry" :entry-point nil)))
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:run-project root "fixnoentry"))
      (check "no :entry-point falls back to src/main:main" code clef-runner:+exit-success+)
      (check-true "  and ran it" (search "args=" out))
      (check-true "  saying it assumed" (search "no :entry-point" err))))

  ;; An entry point that names nothing is a configuration error the user must
  ;; see, and the program must not be pretended to have run.
  (let ((root (make-project "fixbadentry" :entry-point "fixbadentry/src/main:nope")))
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:run-project root "fixbadentry"))
      (check "a missing entry function exits 3" code clef-runner:+exit-diagnostics+)
      (check "  and runs nothing" out "")
      (check-true "  naming the function" (search "NOPE" err))))

  ;; Compile diagnostics from the project's own files render before anything
  ;; runs -- the reason all of this exists.
  (let ((root (make-project "fixwarn"
                            :extra-main "(defun never-called () undefined-thing-xyz)")))
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:run-project root "fixwarn"))
      (check "a warning alone still runs the program" code clef-runner:+exit-success+)
      (check-true "  and the program ran" (search "args=" out))
      (check-true "  the warning was rendered" (search "UNDEFINED-THING-XYZ" (string-upcase err))))
    ;; The fasls are now up to date. A second run must say the same thing:
    ;; the project's own files are recompiled every run, so a warning cannot
    ;; appear once and then vanish, and --werror means the same thing twice.
    ;; (This image has the project loaded already, so the reload here also
    ;; redefines its functions; a real `clef run' is a fresh process and does
    ;; not. Not checked for that reason.)
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:run-project root "fixwarn"))
      (declare (ignore out))
      (check "a second run with fresh fasls exits the same" code clef-runner:+exit-success+)
      (check-true "  and reports the warning again" (search "UNDEFINED-THING-XYZ" (string-upcase err))))
    (multiple-value-bind (code out err)
        (capturing (o e) (let ((clef-runner:*warnings-as-errors* t))
                           (clef-runner:run-project root "fixwarn")))
      (check "under --werror the third run exits 3" code clef-runner:+exit-diagnostics+)
      (check "  and runs nothing" out "")
      (check-true "  naming the warning" (search "UNDEFINED-THING-XYZ" (string-upcase err))))))

(defun test-testing ()
  (format t "~&testing the project~%")
  (let ((root (make-project "fixtest")))
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:test-project root "fixtest"))
      (check "clef test loads the wired module and calls run-tests" code clef-runner:+exit-success+)
      (check-true "  which ran" (search "ok" out))
      (check "  quietly" err "")))

  (let ((root (make-project "fixtestconv" :wire-tests nil)))
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:test-project root "fixtestconv"))
      (check "no test-op wiring falls back to test/main" code clef-runner:+exit-success+)
      (check-true "  and ran it" (search "ok" out))
      (check-true "  saying it assumed" (search "no test-op" err))))

  ;; Several test modules load in one ASDF session, so the second does not
  ;; reload what the first did (the reload bug, w3-migration-trial.md §2.2.8),
  ;; and every RUN-TESTS is called.
  (let ((root (make-project "fixtesttwo" :wire-tests nil)))
    (write-file root (format nil "fixtesttwo.asd")
                "(asdf:defsystem #:fixtesttwo
  :class :package-inferred-system
  :depends-on (\"fixtesttwo/src/main\")
  :in-order-to ((asdf:test-op (asdf:load-op \"fixtesttwo/test/main\")
                              (asdf:load-op \"fixtesttwo/test/more\"))))
")
    (write-file root "test/more.lisp"
                "(defpackage :fixtesttwo/test/more (:use :cl) (:export :run-tests))
(in-package :fixtesttwo/test/more)
(defun run-tests () (format t \"more~%\"))
")
    (multiple-value-bind (code out err)
        (capturing (o e) (clef-runner:test-project root "fixtesttwo"))
      (check "two wired modules both load" code clef-runner:+exit-success+)
      (check-true "  the first ran" (search "ok" out))
      (check-true "  and the second" (search "more" out))
      (check "  with no redefinition noise between them" err "")))

  ;; A failing RUN-TESTS signals; under the runtime that is exit 1. In-image
  ;; the error reaches us instead, which is the same thing one layer down.
  (let ((root (make-project "fixtestfail" :test-body "(error \"2 tests failed\")")))
    (let ((outcome (handler-case
                       (capturing (o e) (clef-runner:test-project root "fixtestfail"))
                     (error (e) (list :died (princ-to-string e))))))
      (check-true "a failing run-tests propagates its error"
                  (and (consp outcome) (eq (first outcome) :died)))
      (check-true "  with the test's own message" (search "2 tests failed" (second outcome))))))

(defun run-project-tests ()
  (test-finding)
  (test-running)
  (test-testing))
