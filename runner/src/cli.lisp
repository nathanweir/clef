(defpackage :clef-runner/src/cli
  (:use :cl)
  (:import-from :clef-conditions)
  (:import-from :clef-runner/src/runtime
                #:+exit-success+
                #:+exit-usage+
                #:*optimize-policy*
                #:*warnings-as-errors*
                #:*min-severity*
                #:with-runtime
                ;; Internal to the component; see compile.lisp.
                #:color-default)
  (:import-from :clef-runner/src/compile
                #:run-file
                #:run-system)
  (:import-from :clef-runner/src/project
                #:find-project
                #:run-project
                #:test-project
                #:call-with-program-argv)
  (:export
   #:main
   #:parse-args
   #:*usage*))

(in-package :clef-runner/src/cli)

;;;; The command line.
;;;;
;;;; Deliberately small. This is the runner's configuration surface -- the thing
;;;; Nathan described as "a unified clef tool could also expose configurations
;;;; around things the user doesn't want" -- and every flag here corresponds to a
;;;; default that SBCL gets wrong for non-interactive use.

(defparameter *usage*
  "clef-run -- run Common Lisp with legible errors and a real exit code

USAGE:
    clef-run [options] <file.lisp> [-- args...]
    clef-run [options] --system <name>
    clef-run [options] [-- args...]

With no file and no --system, runs the golden-path project in the current
directory or above it (the one holding init.lisp and its .asd): the init,
then the system, then the entry point the .asd names. Everything after `--'
is the program's, in UIOP:*COMMAND-LINE-ARGUMENTS*.

OPTIONS:
    --system <name>   load an ASDF system instead of a file
    --release         optimize for speed rather than debuggability
    --werror          treat warnings as errors
    -q, --quiet       report errors only, not warnings or style warnings
    -v, --verbose     report notes as well
    --color           force colour
    --no-color        disable colour (also honours NO_COLOR)
    -h, --help        this text
    --version         print version

EXIT CODES:
    0   ran and returned normally
    1   a serious condition nobody handled
    2   clef-run was invoked wrongly
    3   compilation produced errors, or warnings under --werror
")

(defparameter *version* "0.0.1")

(defstruct options
  (target nil)
  (kind :file)          ; :file, :system, or :project (no target given)
  (argv '())            ; the program's arguments: everything after --
  (command :run)        ; :run or :test -- which verb the umbrella invoked
  (policy :dev)
  (werror nil)
  (min-severity :style-warning)
  (color :auto)         ; :auto :always :never
  (action :run))        ; :run :help :version :usage-error

(defun parse-args (args &optional (command :run))
  "Parse ARGS (not including argv[0]) into an OPTIONS.

COMMAND is :RUN for `clef run' / `clef-run' and :TEST for `clef test', which
takes the same options but no target: its target is always the project.

Unknown flags are a usage error rather than being ignored. Silently accepting a
misspelled flag and running with the wrong settings is exactly the failure mode
this project exists to remove."
  (let ((opts (make-options :command command))
        (message nil))
    (loop while args
          for arg = (pop args)
          do (cond
               ((or (string= arg "-h") (string= arg "--help"))
                (setf (options-action opts) :help)
                (return))
               ((string= arg "--version")
                (setf (options-action opts) :version)
                (return))
               ((string= arg "--system")
                (if args
                    (setf (options-kind opts) :system
                          (options-target opts) (pop args))
                    (setf message "--system needs a system name")))
               ((string= arg "--release") (setf (options-policy opts) :release))
               ((string= arg "--werror") (setf (options-werror opts) t))
               ((or (string= arg "-q") (string= arg "--quiet"))
                (setf (options-min-severity opts) :error))
               ((or (string= arg "-v") (string= arg "--verbose"))
                (setf (options-min-severity opts) :note))
               ((string= arg "--color") (setf (options-color opts) :always))
               ((string= arg "--no-color") (setf (options-color opts) :never))
               ;; "--" ends our options; the rest belongs to the program. A
               ;; file whose own name starts with a dash is spelled ./--name.
               ((string= arg "--")
                (setf (options-argv opts) args
                      args nil))
               ((and (> (length arg) 1) (char= (char arg 0) #\-))
                (setf message (format nil "unknown option: ~A" arg)))
               ((eq command :test)
                (setf message (format nil "clef test takes no target, only options ~
                                           (got ~A); it tests the project here" arg)))
               ((options-target opts)
                (setf message (format nil "unexpected extra argument: ~A" arg)))
               (t (setf (options-target opts) arg)))
          until message)
    (cond
      (message
       (setf (options-action opts) :usage-error)
       (values opts message))
      (t
       ;; No target is not an error here: it means the project at hand. Whether
       ;; there is one is only knowable at run time, so MAIN answers that.
       (when (null (options-target opts))
         (setf (options-kind opts) :project))
       (values opts nil)))))

(defun color-enabled-p (setting)
  (ecase setting
    (:always t)
    (:never nil)
    (:auto (color-default))))

(defun no-project-message (command)
  (format nil "~:[nothing to run: no file given, and~;clef test:~] no golden-path ~
               project here or above (a directory holding init.lisp and one .asd)"
          (eq command :test)))

(defun run-here (opts)
  "Run or test the project at or above the cwd, per OPTS. Returns the exit code."
  (multiple-value-bind (root name) (find-project)
    (cond
      ((null root)
       (format *error-output* "~&error: ~A~%" (no-project-message (options-command opts)))
       +exit-usage+)
      ((eq (options-command opts) :test) (test-project root name))
      (t (run-project root name :argv (options-argv opts))))))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)) (command :run))
  "Entry point. Returns an exit code; the binary's toplevel exits with it.

COMMAND is :RUN or :TEST, the verb the umbrella dispatched. The standalone
clef-run binary only ever passes :RUN.

Returning rather than exiting keeps this testable -- a test can call MAIN and
assert on the code without taking the process down with it."
  (multiple-value-bind (opts message) (parse-args argv command)
    (ecase (options-action opts)
      (:help (write-string *usage* *standard-output*) +exit-success+)
      (:version (format t "~&clef-run ~A~%" *version*) +exit-success+)
      (:usage-error
       (format *error-output* "~&error: ~A~%~%~A" message *usage*)
       +exit-usage+)
      (:run
       (let ((*optimize-policy* (options-policy opts))
             (*warnings-as-errors* (options-werror opts))
             (*min-severity* (options-min-severity opts))
             (clef-conditions:*color* (color-enabled-p (options-color opts)))
             (target (options-target opts))
             (argv (options-argv opts)))
         (with-runtime
           (ecase (options-kind opts)
             (:file (call-with-program-argv target argv
                                            (lambda () (run-file target))))
             (:system (call-with-program-argv target argv
                                              (lambda () (run-system target))))
             (:project (run-here opts)))))))))
