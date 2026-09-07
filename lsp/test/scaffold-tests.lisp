(defpackage :clef-lsp/test/scaffold-tests
  (:use :cl)
  (:import-from :clef-lsp/test/framework
                #:deftest #:assert-true #:assert-nil #:assert-equal #:assert-not-nil)
  (:local-nicknames (:scaffold :clef-lsp/src/scaffold))
  (:export))

(in-package :clef-lsp/test/scaffold-tests)

;;;; Tests for `clef new'.
;;;;
;;;; The one behaviour with teeth: an existing directory is a valid target,
;;;; because people make the folder first and put a README in it, and tools
;;;; disagree about whether that is allowed. What must never happen is a
;;;; template file landing on top of an existing one -- and a refusal must
;;;; leave the directory exactly as it was.

(defparameter *scaffold-fixture-counter* 0)

(defun fresh-root ()
  "A throwaway directory under lsp/tmp/test/, emptied if it exists."
  (let ((root (merge-pathnames
               (format nil "tmp/test/scaffold-~D/" (incf *scaffold-fixture-counter*))
               (asdf:system-source-directory :clef-lsp))))
    (when (probe-file root)
      (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist root)
    root))

(defun touch (path &optional (content ""))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string content out))
  path)

(defun file-under (dir rel) (merge-pathnames rel dir))

(deftest test-new-creates-a-fresh-project
  "A bare name becomes a directory holding the whole template, named by it"
  (let* ((root (fresh-root))
         (dir (scaffold:new-project "myapp" :output-root root)))
    (assert-true (uiop:directory-exists-p dir) "The project directory exists")
    (assert-true (probe-file (file-under dir "myapp.asd")) "The .asd carries the name")
    (assert-true (probe-file (file-under dir "init.lisp")) "The hermetic init is there")
    (assert-not-nil (search "myapp/src/main"
                            (uiop:read-file-string (file-under dir "src/main.lisp")))
                    "Packages are named after the project")))

(deftest test-new-scaffolds-into-an-existing-directory
  "A pre-made folder with its own files is a valid target, and they survive"
  (let* ((root (fresh-root))
         (existing (file-under root "notes/"))
         (docs (touch (file-under existing "docs/plan.md") "the plan")))
    (multiple-value-bind (dir kept) (scaffold:new-project "notes" :output-root root)
      (assert-true (uiop:pathname-equal dir existing) "Scaffolded in place")
      (assert-true (probe-file (file-under dir "notes.asd")) "Template written")
      (assert-equal "the plan" (uiop:read-file-string docs) "The docs tree is untouched")
      (assert-nil kept "Nothing of the template's was skipped"))))

(deftest test-new-keeps-the-users-readme-and-gitignore
  "A README.md or .gitignore already there is kept, reported, and not a collision"
  (let* ((root (fresh-root))
         (existing (file-under root "repo/"))
         (readme (touch (file-under existing "README.md") "# mine"))
         (ignore (touch (file-under existing ".gitignore") "*.fasl")))
    (multiple-value-bind (dir kept) (scaffold:new-project "repo" :output-root root)
      (assert-true (probe-file (file-under dir "repo.asd")) "The rest is written")
      (assert-equal "# mine" (uiop:read-file-string readme) "README kept")
      (assert-equal "*.fasl" (uiop:read-file-string ignore) ".gitignore kept")
      (assert-equal '("README.md" ".gitignore") (sort (copy-list kept) #'string>)
                    "And both are reported as kept"))))

(deftest test-new-refuses-a-collision-and-writes-nothing
  "One existing template file stops the whole scaffold, before any write"
  (let* ((root (fresh-root))
         (existing (file-under root "taken/")))
    (touch (file-under existing "init.lisp") "mine")
    (let ((err (handler-case (progn (scaffold:new-project "taken" :output-root root) nil)
                 (error (e) (princ-to-string e)))))
      (assert-not-nil err "It must signal")
      (assert-not-nil (search "init.lisp" err) "And name the colliding file")
      (assert-equal "mine" (uiop:read-file-string (file-under existing "init.lisp"))
                    "The existing file is intact")
      (assert-nil (probe-file (file-under existing "taken.asd"))
                  "And nothing else was written"))))

(deftest test-new-takes-a-path-and-names-by-its-last-component
  "clef new some/where/tool names the project `tool'"
  (let* ((root (fresh-root))
         (dir (scaffold:new-project "some/where/tool" :output-root root)))
    (assert-true (probe-file (file-under dir "tool.asd")) "Named by the last component")
    (assert-true (probe-file (file-under root "some/where/tool/tool.asd"))
                 "And created at the given path")))

(deftest test-new-dot-means-here
  "clef new . scaffolds into the current directory, named after it"
  (let* ((root (fresh-root))
         (here (file-under root "present/")))
    (ensure-directories-exist here)
    (let ((dir (scaffold:new-project "." :output-root here)))
      (assert-true (uiop:pathname-equal dir here) "The target is the directory itself")
      (assert-true (probe-file (file-under here "present.asd")) "Named after it"))))

(deftest test-new-rejects-a-name-that-cannot-be-a-system
  "The directory's own name must work as a system and package name"
  (let ((root (fresh-root)))
    (assert-not-nil (handler-case (progn (scaffold:new-project "bad_name" :output-root root) nil)
                      (error (e) e))
                    "Underscores are refused")
    (assert-nil (probe-file (file-under root "bad_name/")) "And nothing was created")))
