(in-package :clef-runner)

;;;; Compiling and loading with legible diagnostics.
;;;;
;;;; This is where the runner pays off visibly: instead of SBCL's wall of
;;;; uppercase s-expressions, each condition goes through CLEF-CONDITIONS to
;;;; become a message, a location, and the offending line with the problem
;;;; marked.
;;;;
;;;; The extraction has to happen inside the handler. SBCL's compiler error
;;;; context is dynamic state, not carried on the condition object, so it is gone
;;;; the moment the handler returns.

(defun optimize-declaration (policy)
  "The OPTIMIZE qualities for POLICY.

:DEV asks the compiler for everything it can tell us. (debug 3) keeps full debug
information -- crucially it suppresses tail-call merging, so the functions that
led to a failure are still on the stack to be named -- and (safety 3) keeps the
checks that turn undefined behaviour into a condition. That combination is what
makes the rest of this component useful. SBCL's default sits at 1/1/1, which is
not enough debug information for a usable backtrace and not enough speed to be
worth the loss."
  (ecase policy
    (:dev '(optimize (debug 3) (safety 3) (speed 1) (compilation-speed 1)))
    (:release '(optimize (debug 1) (safety 1) (speed 3) (compilation-speed 0)))))

(defun apply-optimize-policy ()
  "Establish the optimize policy for everything this process compiles.

PROCLAIM, globally, rather than WITH-COMPILATION-UNIT's :POLICY.

The scoped version was tried first and **does not work** -- and fails silently,
which is the failure mode this whole project exists to remove. Measured twice:
docs/experiments/defaults/03-policy-and-frames.lisp found that the wrapped shape
'((optimize ...)) is ignored outright, and 04-runner-policy-check.lisp then found
that even the correct bare shape left the runner compiling at 1/1/1, with zero
user frames surviving on the stack at either :dev or :release. A global PROCLAIM
was the only thing that measurably took effect.

Global is the right scope here anyway, and an earlier draft of this comment had
that backwards. Leaking a policy into unrelated code is a hazard for a *library*
that gets loaded into someone else's image. The runner is not that: it owns the
process and exists to run exactly one program. Code that declaims its own
settings still overrides this, which is correct."
  (proclaim (optimize-declaration *optimize-policy*)))

(defun reportable-p (condition)
  "Is CONDITION something the user should hear about?

SB-C:COMPILER-ERROR is named explicitly because it is NOT a subtype of ERROR --
it is an encapsulated condition, and it is what a read error arrives as. A
handler filtering on (or warning error) drops it silently, and a file with an
unbalanced paren then reports nothing at all."
  (typep condition '(or warning error sb-c:compiler-error)))

(defun muffle (condition)
  "Stop SBCL printing CONDITION itself, since we are printing it better.

Only a WARNING is guaranteed to offer MUFFLE-WARNING; for anything else, or if
some inner handler already took the restart away, this quietly does nothing and
the worst case is that SBCL's version appears alongside ours."
  (let ((restart (find-restart 'muffle-warning condition)))
    (when restart (invoke-restart restart))))

(defun collect-diagnostics (thunk)
  "Call THUNK, returning (values result diagnostics).

DIAGNOSTICS is a list of CLEF-CONDITIONS:DIAGNOSTIC in the order signalled."
  (let ((collected '()))
    (values
     (handler-bind
         ((condition
            (lambda (c)
              (when (reportable-p c)
                (let ((d (ignore-errors (clef-conditions:extract c))))
                  (when d (push d collected)))
                ;; Errors must keep propagating -- muffling one would swallow a
                ;; real failure. Only warnings are suppressed, and only because
                ;; we have already captured them.
                (when (typep c 'warning) (muffle c))))))
       (funcall thunk))
     (nreverse collected))))

(defun report-diagnostics (diagnostics &key (stream nil))
  "Render DIAGNOSTICS and return (values error-count warning-count).

Counts are of what was *found*, not of what was printed -- a diagnostic filtered
out by *MIN-SEVERITY* still counts toward the exit status. Hiding notes should
change what you read, not whether the build passed."
  (let ((out (or stream (diagnostic-stream)))
        (errors 0)
        (warnings 0))
    (dolist (d diagnostics)
      (let ((severity (clef-conditions:diagnostic-severity d)))
        (case severity
          (:error (incf errors))
          (:warning (incf warnings))
          (t nil))
        (when (severity>= severity *min-severity*)
          (clef-conditions:render d :stream out)
          (terpri out))))
    (finish-output out)
    (values errors warnings)))

(defun summarize (errors warnings &key (stream nil))
  "One line saying what happened, or nothing at all when nothing did.

Silence on success is deliberate. A toolchain that congratulates itself on every
run trains you to stop reading its output."
  (let ((out (or stream (diagnostic-stream))))
    (when (or (plusp errors) (plusp warnings))
      (format out "~&~[~:;~:*~D error~:P~]~:[~;, ~]~[~:;~:*~D warning~:P~]~%"
              errors (and (plusp errors) (plusp warnings)) warnings)
      (finish-output out))))

(defun failed-p (errors warnings)
  (or (plusp errors) (and *warnings-as-errors* (plusp warnings))))

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun compile-and-load (path)
  "Compile PATH under the configured optimize policy, then load the result.
Returns (values loaded-p chatter).

Compiling first rather than LOADing the source is what gets compiler diagnostics
at all -- LOAD of a source file evaluates form by form and reports far less.

CHATTER is whatever SBCL printed on its own account. We render every reportable
condition ourselves, so letting SBCL also print its version means reading each
problem twice -- once legibly and once as a wall of uppercase s-expressions. But
discarding it outright would swallow anything we failed to recognise, so it is
captured and the caller replays it only if we came away with nothing.

The capture covers COMPILE-FILE alone, deliberately. Extending it over LOAD would
swallow the program's own writes to *ERROR-OUTPUT*, which are none of our
business."
  (let ((fasl nil)
        (failed nil)
        (chatter ""))
    (apply-optimize-policy)
    (unwind-protect
         (progn
           (setf chatter
                 (with-output-to-string (sink)
                   (let ((*error-output* sink))
                     (multiple-value-bind (output warnings-p failure-p)
                         (compile-file path :verbose nil :print nil)
                       (declare (ignore warnings-p))
                       (setf fasl output
                             failed failure-p)))))
           ;; FAILURE-P means errors, not warnings. Loading the fasl anyway
           ;; produces a second, less informative failure stacked on the first.
           (values (when (and fasl (not failed))
                     (load fasl :verbose nil :print nil)
                     t)
                   chatter))
      (when (and fasl (probe-file fasl))
        (ignore-errors (delete-file fasl))))))

(defun finish (diagnostics chatter stream)
  "Report DIAGNOSTICS to STREAM and return the exit code."
  (multiple-value-bind (errors warnings) (report-diagnostics diagnostics :stream stream)
    ;; Only if we understood nothing at all -- otherwise this is the duplicate
    ;; we went to the trouble of capturing.
    (when (and (null diagnostics) chatter (plusp (length chatter)))
      (write-string chatter stream)
      (finish-output stream))
    (summarize errors warnings :stream stream)
    (if (failed-p errors warnings)
        +exit-diagnostics+
        +exit-success+)))

(defun run-file (path)
  "Compile, load and run PATH. Returns the exit code to use."
  (let ((stream (diagnostic-stream))
        (truename (probe-file path)))
    (if (null truename)
        (progn (format stream "~&error: no such file: ~A~%" path)
               +exit-usage+)
        (let ((chatter ""))
          (multiple-value-bind (result diagnostics)
              (collect-diagnostics
               (lambda ()
                 (multiple-value-bind (loaded text) (compile-and-load truename)
                   (setf chatter text)
                   loaded)))
            (declare (ignore result))
            (finish diagnostics chatter stream))))))

(defun run-system (name)
  "Load ASDF system NAME with the same treatment. Returns the exit code.

No output capture here: ASDF's own progress reporting is already silenced by the
runtime's *COMPILE-VERBOSE* bindings, and a system load runs arbitrary code whose
writes to *ERROR-OUTPUT* are its own business."
  (let ((stream (diagnostic-stream)))
    (multiple-value-bind (result diagnostics)
        (collect-diagnostics
         (lambda ()
           (apply-optimize-policy)
           (asdf:load-system name :verbose nil)))
      (declare (ignore result))
      (finish diagnostics nil stream))))
