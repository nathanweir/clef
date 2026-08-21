(in-package :clef-runner/test)

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
    (check "a bare path is the target" (opt-slot p 'clef-runner::target) "foo.lisp")
    (check "  kind is :file" (opt-slot p 'clef-runner::kind) :file)
    (check "  action is :run" (opt-slot p 'clef-runner::action) :run)
    (check "  no message" (second p) nil))

  (let ((p (opts "--system" "my-app")))
    (check "--system sets the kind" (opt-slot p 'clef-runner::kind) :system)
    (check "  and the target" (opt-slot p 'clef-runner::target) "my-app"))

  (let ((p (opts "--release" "--werror" "foo.lisp")))
    (check "--release" (opt-slot p 'clef-runner::policy) :release)
    (check "--werror" (opt-slot p 'clef-runner::werror) t))

  (let ((p (opts "-q" "foo.lisp")))
    (check "-q raises the severity floor"
           (opt-slot p 'clef-runner::min-severity) :error))
  (let ((p (opts "-v" "foo.lisp")))
    (check "-v lowers it" (opt-slot p 'clef-runner::min-severity) :note))

  ;; Unknown flags must be refused, not ignored.
  (let ((p (opts "--wrror" "foo.lisp")))
    (check "a misspelled flag is a usage error"
           (opt-slot p 'clef-runner::action) :usage-error)
    (check-true "  and says which one" (search "--wrror" (second p))))

  (let ((p (opts)))
    (check "no arguments is a usage error" (opt-slot p 'clef-runner::action) :usage-error))

  (let ((p (opts "--system")))
    (check "--system with no name is a usage error"
           (opt-slot p 'clef-runner::action) :usage-error))

  (let ((p (opts "a.lisp" "b.lisp")))
    (check "two targets is a usage error" (opt-slot p 'clef-runner::action) :usage-error))

  ;; -- ends option parsing, so a file whose name starts with a dash is reachable.
  (let ((p (opts "--" "--weird-name.lisp")))
    (check "-- ends option parsing"
           (opt-slot p 'clef-runner::target) "--weird-name.lisp")
    (check "  and it is a run" (opt-slot p 'clef-runner::action) :run))

  (let ((p (opts "--help")))
    (check "--help" (opt-slot p 'clef-runner::action) :help))
  (let ((p (opts "--version")))
    (check "--version" (opt-slot p 'clef-runner::action) :version))

  (format t "~&main~%")
  ;; MAIN returns a code rather than exiting, which is what makes this testable.
  (check "--help exits 0"
         (let ((*standard-output* (make-broadcast-stream)))
           (clef-runner:main '("--help")))
         0)
  (check "a usage error exits 2"
         (let ((*error-output* (make-broadcast-stream)))
           (clef-runner:main '("--nonsense")))
         clef-runner:+exit-usage+))

(defun run-all-tests ()
  (setf *failures* '() *checks* 0)
  (format t "~&Running clef-runner tests~%~%")
  (run-runtime-tests)
  (run-cli-tests)
  (format t "~&~%~A checks, ~A failure(s)~%" *checks* (length *failures*))
  (null *failures*))
