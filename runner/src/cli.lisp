(in-package :clef-runner)

;;;; The command line.
;;;;
;;;; Deliberately small. This is the runner's configuration surface -- the thing
;;;; Nathan described as "a unified clef tool could also expose configurations
;;;; around things the user doesn't want" -- and every flag here corresponds to a
;;;; default that SBCL gets wrong for non-interactive use.

(defparameter *usage*
  "clef-run -- run Common Lisp with legible errors and a real exit code

USAGE:
    clef-run [options] <file.lisp>
    clef-run [options] --system <name>

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
  (kind :file)          ; :file or :system
  (policy :dev)
  (werror nil)
  (min-severity :style-warning)
  (color :auto)         ; :auto :always :never
  (action :run))        ; :run :help :version :usage-error

(defun parse-args (args)
  "Parse ARGS (not including argv[0]) into an OPTIONS.

Unknown flags are a usage error rather than being ignored. Silently accepting a
misspelled flag and running with the wrong settings is exactly the failure mode
this project exists to remove."
  (let ((opts (make-options))
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
               ;; "--" ends option parsing, so a file called --weird is reachable.
               ((string= arg "--")
                (when args (setf (options-target opts) (pop args))))
               ((and (> (length arg) 1) (char= (char arg 0) #\-))
                (setf message (format nil "unknown option: ~A" arg)))
               ((options-target opts)
                (setf message (format nil "unexpected extra argument: ~A" arg)))
               (t (setf (options-target opts) arg)))
          until message)
    (cond
      (message
       (setf (options-action opts) :usage-error)
       (values opts message))
      ((and (eq (options-action opts) :run) (null (options-target opts)))
       (setf (options-action opts) :usage-error)
       (values opts "nothing to run"))
      (t (values opts nil)))))

(defun color-enabled-p (setting)
  (ecase setting
    (:always t)
    (:never nil)
    (:auto (color-default))))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  "Entry point. Returns an exit code; the binary's toplevel exits with it.

Returning rather than exiting keeps this testable -- a test can call MAIN and
assert on the code without taking the process down with it."
  (multiple-value-bind (opts message) (parse-args argv)
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
             (clef-conditions:*color* (color-enabled-p (options-color opts))))
         (with-runtime
           (ecase (options-kind opts)
             (:file (run-file (options-target opts)))
             (:system (run-system (options-target opts))))))))))
