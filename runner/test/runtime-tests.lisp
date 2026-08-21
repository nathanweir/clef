(in-package :clef-runner/test)

;;;; Tests for the runner's process-level guarantees.
;;;;
;;;; The debugger guarantee cannot be tested in-process -- verifying it means
;;;; watching a process die with a particular exit code -- so those cases run in
;;;; subprocesses. Everything else is a plain in-process call.

(defvar *failures* '())
(defvar *checks* 0)

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (if (funcall test got expected)
      (format t "  ~C[32m✓~C[0m ~A~%" #\Escape #\Escape label)
      (progn
        (push (format nil "~A: expected ~S, got ~S" label expected got) *failures*)
        (format t "  ~C[31m✗~C[0m ~A: expected ~S, got ~S~%"
                #\Escape #\Escape label expected got))))

(defun check-true (label got)
  (check label (and got t) t))

(defun temp-source (name text)
  "Write TEXT to a project-local scratch file and return its path.

Project-local, not /tmp: the sandboxed environments this runs in do not have a
writable global temp directory."
  (let ((path (merge-pathnames (format nil "tmp/test/runner-~A.lisp" name)
                               (asdf:system-source-directory :clef-runner))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

;;; ---------------------------------------------------------------------------
;;; Exit-code contract
;;; ---------------------------------------------------------------------------

(defun test-exit-codes ()
  (format t "~&exit-code contract~%")
  ;; Fixed values, not just distinct ones. Scripts branch on these numbers, so
  ;; changing one is a breaking change and should require editing this test.
  (check "success" clef-runner:+exit-success+ 0)
  (check "failure" clef-runner:+exit-failure+ 1)
  (check "usage" clef-runner:+exit-usage+ 2)
  (check "diagnostics" clef-runner:+exit-diagnostics+ 3))

;;; ---------------------------------------------------------------------------
;;; Diagnostics
;;; ---------------------------------------------------------------------------

(defun test-clean-file ()
  (format t "~&clean file~%")
  (let* ((path (temp-source "clean" "(defun runner-clean-fn (x) (+ x 1))
"))
         (out (make-string-output-stream))
         (code (let ((clef-runner:*diagnostic-stream* out))
                 (clef-runner:run-file path))))
    (check "exits 0" code 0)
    ;; Silence on success is a deliberate design choice, so it gets a test.
    (check "says nothing" (get-output-stream-string out) "")))

(defun test-warnings-are-reported ()
  (format t "~&warnings~%")
  (let* ((path (temp-source "unused" "(defun runner-unused-fn (never-used) 42)
"))
         (out (make-string-output-stream))
         (code (let ((clef-runner:*diagnostic-stream* out))
                 (clef-runner:run-file path)))
         (text (get-output-stream-string out)))
    ;; A style warning alone must NOT fail the run.
    (check "style warning does not fail the run" code 0)
    (check-true "names the variable" (search "NEVER-USED" text))
    (check-true "shows the source line" (search "defun runner-unused-fn" text))
    ;; The whole point: a location, not just a message.
    (check-true "points at a line and column" (search "-->" text))
    ;; And no colour, because the stream is not a terminal.
    (check "no escape codes" (position #\Escape text) nil)))

(defun test-errors-fail-the-run ()
  (format t "~&errors~%")
  (let* ((path (temp-source "bad-package" "(defun runner-bad () (no-such-pkg-abc:g 1))
"))
         (out (make-string-output-stream))
         (code (let ((clef-runner:*diagnostic-stream* out))
                 (clef-runner:run-file path)))
         (text (get-output-stream-string out)))
    (check "read error fails the run" code clef-runner:+exit-diagnostics+)
    (check-true "names the package" (search "NO-SUCH-PKG-ABC" (string-upcase text)))
    ;; SBCL's own report appends the stream object; ours must not.
    (check "no stream object in the message"
           (search "FORM-TRACKING-STREAM" text) nil)
    ;; SBCL prints its own version of every caught condition. Since we render
    ;; each one legibly, letting its wall through means reading the same problem
    ;; twice -- so the compile's own output is captured and dropped.
    (check "SBCL's own wall is not printed alongside"
           (search "caught ERROR" text) nil)))

(defun test-werror ()
  (format t "~&--werror~%")
  (let* ((path (temp-source "arity" "(defun runner-two (a b) (list a b))
(defun runner-caller () (runner-two 1 2 3))
"))
         (out (make-string-output-stream)))
    (check "warning passes by default"
           (let ((clef-runner:*diagnostic-stream* out)
                 (clef-runner:*warnings-as-errors* nil))
             (clef-runner:run-file path))
           0)
    (check "warning fails under --werror"
           (let ((clef-runner:*diagnostic-stream* out)
                 (clef-runner:*warnings-as-errors* t))
             (clef-runner:run-file path))
           clef-runner:+exit-diagnostics+)))

(defun test-min-severity-does-not-change-exit-status ()
  (format t "~&severity filtering~%")
  ;; Hiding a diagnostic should change what you read, not whether the build
  ;; passed. Getting this backwards would make -q a way to accidentally pass.
  (let* ((path (temp-source "quiet-arity" "(defun runner-q-two (a b) (list a b))
(defun runner-q-caller () (runner-q-two 1 2 3))
"))
         (out (make-string-output-stream))
         (code (let ((clef-runner:*diagnostic-stream* out)
                     (clef-runner:*warnings-as-errors* t)
                     (clef-runner:*min-severity* :error))
                 (clef-runner:run-file path)))
         (text (get-output-stream-string out)))
    (check "still fails under --werror -q" code clef-runner:+exit-diagnostics+)
    (check "but the warning is not printed" (search "IS CALLED WITH" (string-upcase text)) nil)))

(defun test-missing-file ()
  (format t "~&missing file~%")
  (let* ((out (make-string-output-stream))
         (code (let ((clef-runner:*diagnostic-stream* out))
                 (clef-runner:run-file "no/such/file-xyz.lisp"))))
    (check "usage error, not failure" code clef-runner:+exit-usage+)))

;;; ---------------------------------------------------------------------------
;;; Optimize policy
;;;
;;; This exists because the first two attempts at setting the policy silently
;;; did nothing, which is precisely the failure mode the project exists to
;;; remove. WITH-COMPILATION-UNIT's :POLICY was ignored both when given the
;;; wrapped shape '((optimize ...)) and when given the correct bare shape --
;;; measured in docs/experiments/defaults/03-policy-and-frames.lisp and
;;; 04-runner-policy-check.lisp.
;;;
;;; So the test is of the observable consequence, not of the setting. (debug 3)
;;; suppresses tail-call merging, so the functions that led to a failure survive
;;; as frames. If the policy stops taking effect, they vanish and this fails.
;;; ---------------------------------------------------------------------------

(defparameter *chain-source* "(defpackage :runner-chain (:use :cl))
(in-package :runner-chain)
(defun inner (x) (/ 10 x))
(defun middle (x) (inner x))
(defun outer (x) (middle x))
")

(defun user-frame-count (policy name)
  "Compile the chain under POLICY, provoke it, and count its own frames."
  (when (find-package :runner-chain) (delete-package :runner-chain))
  (let ((clef-runner::*optimize-policy* policy)
        (*error-output* (make-broadcast-stream)))
    (clef-runner::compile-and-load (temp-source name *chain-source*)))
  (let ((frames '()))
    ;; HANDLER-CASE OUTSIDE, HANDLER-BIND INSIDE. Nesting these the other way
    ;; makes the handler never run and every policy look identical.
    (handler-case
        (handler-bind
            ((error (lambda (c)
                      (declare (ignore c))
                      (setf frames
                            (uiop:split-string
                             (string-right-trim
                              '(#\Newline)
                              (with-output-to-string (s)
                                (sb-debug:print-backtrace :stream s :count 30
                                                          :print-thread nil)))
                             :separator '(#\Newline))))))
          (funcall (find-symbol "OUTER" :runner-chain) 0))
      (error () nil))
    (count-if (lambda (l) (search "RUNNER-CHAIN" l)) frames)))

(defun test-optimize-policy ()
  (format t "~&optimize policy~%")
  ;; The shape matters: a DECLARE-style wrapping is what was silently ignored.
  (check "dev policy is a bare (optimize ...) form"
         (first (clef-runner::optimize-declaration :dev)) 'optimize)
  (check ":dev keeps the calling functions on the stack"
         (user-frame-count :dev "chain-dev") 3)
  ;; The contrast proves the setting is what is responsible, rather than the
  ;; frames happening to survive for some other reason.
  (check ":release lets them be merged away"
         (user-frame-count :release "chain-release") 0))

;;; ---------------------------------------------------------------------------
;;; Backtrace filtering
;;; ---------------------------------------------------------------------------

(defun test-backtrace-filter ()
  (format t "~&backtrace filtering~%")
  ;; A closure's frame has to be judged by the function it is :IN, not by the
  ;; word LAMBDA -- otherwise every closure in SBCL's loader survives the filter.
  (check "a closure is judged by its :IN"
         (clef-runner::frame-head "((LAMBDA NIL :IN SB-FASL::LOAD-AS-FASL))")
         "SB-FASL::LOAD-AS-FASL")
  (check "so is a local function"
         (clef-runner::frame-head "((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))")
         "SB-IMPL::START-LISP")
  (check "a method is judged by its name"
         (clef-runner::frame-head "((:METHOD ASDF/OPERATE:OPERATE (T T)) X)")
         "ASDF/OPERATE:OPERATE")
  (check "a plain call is judged by itself"
         (clef-runner::frame-head "(RUNNER-CHAIN::INNER 0)")
         "RUNNER-CHAIN::INNER")

  (check-true "SBCL internals are noise"
              (clef-runner::noise-frame-p "3: (SB-KERNEL::INTEGER-/-INTEGER 10 0)"))
  (check-true "the runner's own frames are noise"
              (clef-runner::noise-frame-p "5: ((LAMBDA NIL :IN CLEF-RUNNER:RUN-FILE))"))
  (check-true "getting-here functions are noise"
              (clef-runner::noise-frame-p "2: (INVOKE-DEBUGGER #<DIVISION-BY-ZERO>)"))
  (check-true "a load-time toplevel marker is noise"
              (clef-runner::noise-frame-p "7: (\"top level form\") [toplevel]"))
  ;; And the guard against over-filtering: matched exactly, so a user function
  ;; whose name merely starts with a noise name survives.
  (check "a user function named ERROR-REPORTER survives"
         (clef-runner::noise-frame-p "4: (MY-APP::ERROR-REPORTER 1)") nil)
  (check "and one merely containing SB- survives"
         (clef-runner::noise-frame-p "4: (MY-APP::MAKE-SB-THING 1)") nil))

;;; ---------------------------------------------------------------------------
;;; The debugger guarantee, in subprocesses
;;;
;;; Mirrors docs/experiments/defaults/02-handler-reinstalls-hook.lisp, but
;;; against the real CLEF-RUNNER:WITH-RUNTIME rather than a sketch of it. These
;;; are the cases that justify the component existing at all, so they are worth
;;; the cost of spawning processes.
;;; ---------------------------------------------------------------------------

(defun run-in-subprocess (form)
  "Evaluate FORM in a fresh SBCL with clef-runner loaded. Returns the exit code."
  (let* ((root (asdf:system-source-directory :clef-runner))
         (repo (make-pathname :directory (butlast (pathname-directory root))))
         (setup (temp-source
                 "subprocess-setup"
                 (format nil "(require :asdf)
(asdf:initialize-output-translations
 '(:output-translations ((~S :**/ :*.*.*) (~S \"build\" :**/ :*.*.*))
   :inherit-configuration))
(asdf:load-asd ~S)
(asdf:load-asd ~S)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :clef-runner :verbose nil))
"
                         (namestring repo) (namestring repo)
                         (namestring (merge-pathnames "conditions/clef-conditions.asd" repo))
                         (namestring (merge-pathnames "clef-runner.asd" root)))))
         (proc (sb-ext:run-program
                "sbcl"
                (list "--noinform" "--disable-debugger"
                      "--load" (namestring setup)
                      "--eval" form
                      "--eval" "(sb-ext:exit :code 0)")
                :search t :input nil
                :output nil :error nil :wait t)))
    (sb-ext:process-exit-code proc)))

(defun test-debugger-guarantee ()
  (format t "~&debugger guarantee (subprocesses)~%")

  ;; The case --disable-debugger cannot handle: hostile code rebinds the hook,
  ;; and the process reaches the debugger, hits EOF, and exits ZERO.
  (check "survives a hostile rebind of the debugger hook"
         (run-in-subprocess
          "(clef-runner:with-runtime
             (let ((sb-ext:*invoke-debugger-hook* nil))
               (error \"boom\")))")
         clef-runner:+exit-failure+)

  (check "survives hostile code installing its own hook"
         (run-in-subprocess
          "(clef-runner:with-runtime
             (let ((sb-ext:*invoke-debugger-hook*
                     (lambda (c h) (declare (ignore c h)) (sb-ext:exit :code 42 :abort t))))
               (error \"boom\")))")
         clef-runner:+exit-failure+)

  ;; The regression guard. HANDLER-BIND runs for every SIGNAL, so a runner whose
  ;; outer handler exited would kill a program that merely signalled an advisory
  ;; serious condition and expected it to be declined.
  (check "a bare SIGNAL of a serious condition is left alone"
         (run-in-subprocess
          "(clef-runner:with-runtime
             (signal (make-condition 'simple-error :format-control \"advisory\"))
             (sb-ext:exit :code 0))")
         0)

  ;; And we must not steal errors the program handles itself.
  (check "an inner HANDLER-CASE still wins"
         (run-in-subprocess
          "(clef-runner:with-runtime
             (handler-case (error \"inner\") (error () nil))
             (sb-ext:exit :code 0))")
         0))

(defun run-runtime-tests ()
  (test-exit-codes)
  (test-clean-file)
  (test-warnings-are-reported)
  (test-errors-fail-the-run)
  (test-werror)
  (test-min-severity-does-not-change-exit-status)
  (test-missing-file)
  (test-optimize-policy)
  (test-backtrace-filter)
  (test-debugger-guarantee))
