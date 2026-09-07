(defpackage :clef-runner/test/cli-tests
  (:use :cl)
  (:import-from :clef-runner)
  ;; The OPTIONS slot names, for reading a parse result back by slot.
  (:import-from :clef-runner/src/cli
                #:target #:kind #:argv #:command #:policy #:werror #:min-severity #:action)
  (:import-from :clef-runner/test/harness #:check #:check-true)
  (:export #:run-cli-tests))

(in-package :clef-runner/test/cli-tests)

;;;; Argument parsing.
;;;;
;;;; Cheap to test and worth testing: a runner that silently accepts a
;;;; misspelled flag and runs with the wrong settings is the exact failure mode
;;;; this project exists to remove.

(defun opts (&rest args)
  (multiple-value-list (clef-runner:parse-args args)))

(defun opt-slot (parsed name)
  (slot-value (first parsed) name))

(defun run-cli-tests ()
  (format t "~&argument parsing~%")

  (let ((p (opts "foo.lisp")))
    (check "a bare path is the target" (opt-slot p 'target) "foo.lisp")
    (check "  kind is :file" (opt-slot p 'kind) :file)
    (check "  action is :run" (opt-slot p 'action) :run)
    (check "  no message" (second p) nil))

  (let ((p (opts "--system" "my-app")))
    (check "--system sets the kind" (opt-slot p 'kind) :system)
    (check "  and the target" (opt-slot p 'target) "my-app"))

  (let ((p (opts "--release" "--werror" "foo.lisp")))
    (check "--release" (opt-slot p 'policy) :release)
    (check "--werror" (opt-slot p 'werror) t))

  (let ((p (opts "-q" "foo.lisp")))
    (check "-q raises the severity floor"
           (opt-slot p 'min-severity) :error))
  (let ((p (opts "-v" "foo.lisp")))
    (check "-v lowers it" (opt-slot p 'min-severity) :note))

  ;; Unknown flags must be refused, not ignored.
  (let ((p (opts "--wrror" "foo.lisp")))
    (check "a misspelled flag is a usage error"
           (opt-slot p 'action) :usage-error)
    (check-true "  and says which one" (search "--wrror" (second p))))

  ;; No target means the project at hand; whether there is one is MAIN's
  ;; business, so parsing alone cannot call it an error.
  (let ((p (opts)))
    (check "no arguments means the project here" (opt-slot p 'kind) :project)
    (check "  and is a run" (opt-slot p 'action) :run))

  (let ((p (opts "--system")))
    (check "--system with no name is a usage error"
           (opt-slot p 'action) :usage-error))

  (let ((p (opts "a.lisp" "b.lisp")))
    (check "two targets is a usage error" (opt-slot p 'action) :usage-error))

  ;; -- ends our options; everything after it is the program's.
  (let ((p (opts "--" "a" "--flag" "c")))
    (check "-- hands the rest to the program" (opt-slot p 'argv) '("a" "--flag" "c"))
    (check "  and the run is the project" (opt-slot p 'kind) :project))
  (let ((p (opts "foo.lisp" "--" "x")))
    (check "a file may take arguments too" (opt-slot p 'target) "foo.lisp")
    (check "  after the --" (opt-slot p 'argv) '("x")))
  (let ((p (opts "--werror" "--")))
    (check "an empty -- is fine" (opt-slot p 'argv) '())
    (check "  and options before it still count" (opt-slot p 'werror) t))

  ;; clef test: same options, never a target.
  (let ((p (multiple-value-list (clef-runner:parse-args '("--werror") :test))))
    (check "clef test parses options" (opt-slot p 'werror) t)
    (check "  and is the project" (opt-slot p 'kind) :project)
    (check "  under the test command" (opt-slot p 'command) :test))
  (let ((p (multiple-value-list (clef-runner:parse-args '("foo.lisp") :test))))
    (check "clef test refuses a target" (opt-slot p 'action) :usage-error)
    (check-true "  and says so" (search "clef test" (second p))))

  (let ((p (opts "--help")))
    (check "--help" (opt-slot p 'action) :help))
  (let ((p (opts "--version")))
    (check "--version" (opt-slot p 'action) :version))

  (format t "~&main~%")
  ;; MAIN returns a code rather than exiting, which is what makes this testable.
  (check "--help exits 0"
         (let ((*standard-output* (make-broadcast-stream)))
           (clef-runner:main '("--help")))
         0)
  (check "a usage error exits 2"
         (let ((*error-output* (make-broadcast-stream)))
           (clef-runner:main '("--nonsense")))
         clef-runner:+exit-usage+)
  ;; The suite runs from the repo, which is nobody's golden-path project: no
  ;; init.lisp anywhere up to its .git. So "run the project here" has nothing
  ;; to run, and that is a usage error like any other.
  (let* ((err (make-string-output-stream))
         (code (let ((*error-output* err)) (clef-runner:main '()))))
    (check "no target outside a project exits 2" code clef-runner:+exit-usage+)
    (check-true "  and explains what a project is"
                (search "init.lisp" (get-output-stream-string err))))
  (let* ((err (make-string-output-stream))
         (code (let ((*error-output* err)) (clef-runner:main '() :test))))
    (check "clef test outside a project exits 2" code clef-runner:+exit-usage+)
    (check-true "  naming the command" (search "clef test" (get-output-stream-string err)))))
