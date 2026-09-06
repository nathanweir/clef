(in-package :clef-test)

;;;; Tests for the convention linter.
;;;;
;;;; Each rule guards against a failure measured in docs/surveys/w3-packages.md
;;;; before the rule was written; the tests pin both directions -- the finding
;;;; fires on the violation and stays quiet on conforming code.

(defparameter *lint-fixture-counter* 0)

(defun make-lint-project (files &key (asd-content nil asd-supplied-p))
  "Create a throwaway project directory from FILES, an alist of
relative-path -> content. Returns the project root. The default .asd declares
package-inferred-system, which is the linter's gate."
  (let* ((root (merge-pathnames
                (format nil "tmp/test/lint-~D/" (incf *lint-fixture-counter*))
                (asdf:system-source-directory :clef-lsp))))
    (when (probe-file root)
      (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist root)
    (with-open-file (out (merge-pathnames "fixture.asd" root)
                         :direction :output :if-exists :supersede)
      (write-string (if asd-supplied-p
                        asd-content
                        "(asdf:defsystem \"fixture\"
  :class :package-inferred-system
  :depends-on (\"fixture/src/main\"))")
                    out))
    (dolist (entry files)
      (let ((path (merge-pathnames (car entry) root)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede)
          (write-string (cdr entry) out))))
    root))

(defun lint-kinds (root)
  "The kinds of every finding for ROOT, in order."
  (mapcar #'clef-conditions:diagnostic-kind (clef-lint:lint-project root)))

(deftest test-lint-passes-conforming-code
  "A project that follows the convention produces no findings"
  (let ((root (make-lint-project
               '(("src/main.lisp" . "(defpackage :fixture/src/main
  (:use :cl)
  (:import-from :alexandria :when-let)
  (:local-nicknames (:util :fixture/src/util)))
(in-package :fixture/src/main)")
                 ("src/util.lisp" . "(defpackage :fixture/src/util
  (:use :cl))
(in-package :fixture/src/util)")))))
    (assert-nil (lint-kinds root) "Conforming code must lint clean")))

(deftest test-lint-catches-the-silent-name-mismatch
  "The E2a/E2b killer: package name disagrees with the file's path"
  (let ((root (make-lint-project
               '(("src/main.lisp" . "(defpackage :fixture/src/wrong-name
  (:use :cl))
(in-package :fixture/src/wrong-name)")))))
    (let ((findings (clef-lint:lint-project root)))
      (assert-equal '(:lint-name-mismatch) (mapcar #'clef-conditions:diagnostic-kind findings)
                    "Exactly the mismatch, nothing else")
      ;; The message must say what the path DEMANDS, since that is the fix.
      (assert-not-nil (search "fixture/src/main"
                              (clef-conditions:diagnostic-message (first findings)))
                      "The finding must name the expected package"))))

(deftest test-lint-flags-use-beyond-cl
  "The :USE discipline: :cl passes, anything else is named"
  (let ((root (make-lint-project
               '(("src/main.lisp" . "(defpackage :fixture/src/main
  (:use :cl :alexandria))
(in-package :fixture/src/main)")))))
    (assert-equal '(:lint-use-discipline) (lint-kinds root)
                  ":USE :ALEXANDRIA is the finding; :USE :CL is not")))

(deftest test-lint-names-an-import-cycle-in-order
  "Cycles are reported as a chain of names, not an ASDF op dump"
  (let ((root (make-lint-project
               '(("src/main.lisp" . "(defpackage :fixture/src/main
  (:use :cl)
  (:local-nicknames (:a :fixture/src/a)))
(in-package :fixture/src/main)")
                 ("src/a.lisp" . "(defpackage :fixture/src/a
  (:use :cl)
  (:local-nicknames (:b :fixture/src/b)))
(in-package :fixture/src/a)")
                 ("src/b.lisp" . "(defpackage :fixture/src/b
  (:use :cl)
  (:local-nicknames (:a :fixture/src/a)))
(in-package :fixture/src/b)")))))
    (let* ((findings (clef-lint:lint-project root))
           (cycle (find :lint-import-cycle findings
                        :key #'clef-conditions:diagnostic-kind)))
      (assert-not-nil cycle "The a<->b cycle must be found")
      (assert-not-nil (search "fixture/src/a -> fixture/src/b -> fixture/src/a"
                              (clef-conditions:diagnostic-message cycle))
                      "The chain reads in visit order, ending where it began")
      (assert-equal 1 (count :lint-import-cycle findings
                             :key #'clef-conditions:diagnostic-kind)
                    "One cycle, one finding -- not once per participant"))))

(deftest test-lint-requires-a-leading-defpackage
  "A module file with no defpackage is flagged; root init.lisp is reserved"
  (let ((root (make-lint-project
               '(("src/main.lisp" . "(in-package :cl-user)
(defun orphan () 1)")
                 ("init.lisp" . "(require :asdf)")))))
    (assert-equal '(:lint-no-defpackage) (lint-kinds root)
                  "src/main.lisp is a module and must declare; init.lisp is tooling")))

(deftest test-lint-declines-non-convention-projects
  "A classic-.asd project is not lint territory"
  (let ((root (make-lint-project
               '(("src/main.lisp" . "(in-package :cl-user)"))
               :asd-content "(asdf:defsystem \"fixture\"
  :serial t
  :components ((:file \"src/main\")))")))
    (multiple-value-bind (findings reason) (clef-lint:lint-project root)
      (assert-nil findings "No findings for a project outside the convention")
      (assert-equal :not-a-convention-project reason
                    "And the caller is told why"))))
