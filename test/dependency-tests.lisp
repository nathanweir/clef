(in-package :clef-test)

;;; Dependency tests: ASDF :depends-on forms.
;;;
;;; These exist because the lifecycle tests all point rootUri at
;;; file:///tmp/test-workspace, which has no .asd in it. That means .asd
;;; discovery, dependency parsing and the project symbol map were never
;;; entered by any test, and a type error on a perfectly valid dependency
;;; form silently disabled completion, workspace symbols and cross-file
;;; definition for the entire project.

(defun dep-name (dep)
  (clef-symbols:normalize-dependency-name dep))

(deftest test-dependency-plain-string
  "A plain string dependency yields its name"
  (assert-equal "uiop" (dep-name "uiop")))

(deftest test-dependency-symbol
  "A symbol dependency yields its name"
  (assert-equal "UIOP" (dep-name '|UIOP|)))

(deftest test-dependency-keyword
  "A keyword dependency yields its name"
  (assert-equal "UIOP" (dep-name :uiop)))

(deftest test-dependency-require-form
  "(:require module) names the module -- the SBCL contrib idiom"
  (assert-equal "SB-POSIX" (dep-name '(:require :sb-posix)))
  (assert-equal "sb-bsd-sockets" (dep-name '(:require "sb-bsd-sockets"))))

(deftest test-dependency-version-form
  "(:version name spec) names the system, not the version"
  (assert-equal "alexandria" (dep-name '(:version "alexandria" "1.0.0")))
  (assert-equal "UIOP" (dep-name '(:version :uiop "3.3.0"))))

(deftest test-dependency-feature-form
  "(:feature expr dep) names the nested dependency"
  (assert-equal "uiop" (dep-name '(:feature :sbcl "uiop")))
  (assert-equal "uiop" (dep-name '(:feature (:or :sbcl :ccl) "uiop"))))

(deftest test-dependency-nested-feature-version
  "A :feature wrapping a :version resolves through both levels"
  (assert-equal "alexandria"
                (dep-name '(:feature :sbcl (:version "alexandria" "1.0.0")))))

(deftest test-dependency-unknown-form-is-ignored
  "An unrecognised form yields NIL instead of signalling"
  (assert-nil (dep-name '(:no-such-form "x")))
  (assert-nil (dep-name '()))
  (assert-nil (dep-name 42)))

(deftest test-dependency-forms-do-not-signal
  "No dependency form may signal -- one that does takes the whole symbol map
   down, and the error is swallowed by initialize's handler-case."
  (dolist (dep (list "uiop" :uiop '(:require :sb-posix)
                     '(:version "alexandria" "1.0.0")
                     '(:feature :sbcl "uiop")
                     '(:feature (:or :sbcl :ccl) (:version "uiop" "3.3.0"))
                     '(:bogus) '() 42 #(1 2 3)))
    (handler-case
      (dep-name dep)
      (error (e)
             (assert-true nil (format nil "~S signalled: ~A" dep e))))))

(deftest test-parse-lib-names-survives-exotic-deps
  "parse-lib-names-from-asd returns the real names for list dependency forms.

   This is the end-to-end shape of the bug: one such entry aborted the whole
   project symbol map, so completion and workspace symbols returned nothing."
  (with-direct-handler-test
    (setf (gethash "depbug" clef-context:loaded-systems)
          (clef-symbols:make-system-info
            :name "depbug"
            :asd-path nil
            :dependencies (list "uiop"
                                '(:require :sb-posix)
                                '(:version "alexandria" "1.0.0")
                                '(:feature :sbcl "cl-ppcre"))
            :source-files nil
            :loaded-p nil))
    (let ((names (mapcar #'string-downcase
                         (mapcar #'symbol-name
                                 (clef-symbols:parse-lib-names-from-asd)))))
      (assert-true (member "uiop" names :test #'string=)
                   (format nil "expected uiop in ~S" names))
      (assert-true (member "sb-posix" names :test #'string=)
                   (format nil "expected sb-posix in ~S" names))
      (assert-true (member "alexandria" names :test #'string=)
                   (format nil "expected alexandria in ~S" names))
      (assert-true (member "cl-ppcre" names :test #'string=)
                   (format nil "expected cl-ppcre in ~S" names)))))

(deftest test-safe-load-system-does-not-exit
  "A failing system load must not terminate the process.

   safe-load-system used to bind *invoke-debugger-hook* to something calling
   sb-ext:exit, so a bad .asd in a user's project killed the server from inside
   a request handler. If that regresses, this test run dies outright rather
   than failing -- which is itself the signal."
  (assert-nil (clef-lsp/lifecycle::safe-load-system
                :clef-no-such-system-should-ever-exist)
              "Loading a nonexistent system should return NIL"))
