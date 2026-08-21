(in-package :clef-runner)

;;;; The runner: process-level guarantees a library cannot give itself.
;;;;
;;;; Survey W0 §3 concluded that most of what makes SBCL hostile to
;;;; non-interactive use has to live in the runner rather than in a loadable
;;;; profile, and the reason is measured rather than assumed: --disable-debugger
;;;; is implemented as a *value* in SB-EXT:*INVOKE-DEBUGGER-HOOK*, so any code
;;;; loaded afterwards can rebind it and reach the debugger anyway -- where it
;;;; hits EOF on stdin and exits *zero*. A CI job whose build collapsed reports
;;;; success. See docs/experiments/defaults/01-debugger-escape-paths.lisp,
;;;; cases 5 and 6.
;;;;
;;;; Anything established merely by being loaded can be dismantled by whatever
;;;; loads next. Wrapping cannot.

;;; ---------------------------------------------------------------------------
;;; Exit-code contract
;;;
;;; Small and fixed, because the whole point is that a script can branch on it.
;;; ---------------------------------------------------------------------------

(defconstant +exit-success+ 0
  "The program ran and returned normally.")

(defconstant +exit-failure+ 1
  "The program signalled a serious condition nobody handled.")

(defconstant +exit-usage+ 2
  "clef itself was invoked wrongly. Distinct from +EXIT-FAILURE+ so a script can
tell 'I called this wrong' from 'the program failed'.")

(defconstant +exit-diagnostics+ 3
  "Compilation produced errors, or warnings under --werror. The program was
never run.")

;;; ---------------------------------------------------------------------------
;;; Knobs
;;; ---------------------------------------------------------------------------

(defparameter *optimize-policy* :dev
  "Either :DEV or :RELEASE.

:DEV asks for the compiler's help -- full debug information, full safety
checks -- because a runner whose errors are legible is the entire point of W0.
:RELEASE trades that for speed. Neither is SBCL's default, which sits in the
middle and is the worst of both for development.")

(defparameter *warnings-as-errors* nil
  "When true, a WARNING (not a STYLE-WARNING) makes the run fail with
+EXIT-DIAGNOSTICS+.")

(defparameter *min-severity* :style-warning
  "Least severe diagnostic to report. One of :error :warning :style-warning :note.")

(defparameter *diagnostic-stream* nil
  "Where diagnostics go. NIL means *ERROR-OUTPUT* at the time of use.

Deliberately not defaulted to the *ERROR-OUTPUT* captured at load time: the LSP
rebinds these streams, and a runner that cached one would write into whatever
was current when it was compiled.")

(defparameter +severity-order+ '(:note :style-warning :warning :error)
  "Increasing severity, for comparing against *MIN-SEVERITY*.")

(defun severity>= (a b)
  (>= (or (position a +severity-order+) 0)
      (or (position b +severity-order+) 0)))

(defun diagnostic-stream ()
  (or *diagnostic-stream* *error-output*))

;;; ---------------------------------------------------------------------------
;;; Colour
;;;
;;; Auto-detected, and NO_COLOR is honoured because it is an actual convention
;;; rather than something invented here. Explicit --color/--no-color wins.
;;; ---------------------------------------------------------------------------

(defun tty-p (stream)
  "Is STREAM attached to a terminal? NIL if we cannot tell."
  (or (ignore-errors
       (let ((fd (sb-sys:fd-stream-fd stream)))
         (and fd (plusp (sb-unix:unix-isatty fd)))))
      nil))

(defun color-default ()
  (and (null (uiop:getenv "NO_COLOR"))
       (tty-p (diagnostic-stream))))

;;; ---------------------------------------------------------------------------
;;; The debugger guarantee
;;;
;;; Probe: docs/experiments/defaults/02-handler-reinstalls-hook.lisp.
;;;
;;; The obvious design -- an outer HANDLER-BIND that renders the condition and
;;; exits -- over-reaches, and would break correct programs. HANDLER-BIND runs
;;; for every SIGNAL, not only for calls heading to the debugger, and
;;;
;;;     (signal (make-condition 'simple-error :format-control "advisory"))
;;;
;;; is entitled to return NIL and carry on. Exiting there kills a program that
;;; was working.
;;;
;;; So the handler does not exit. It re-installs the debugger hook and then
;;; DECLINES. Because it runs during the signal, it is already inside the extent
;;; of any hostile LET binding, so its SETF lands on that binding -- and if the
;;; condition really is heading for the debugger, ours is the hook
;;; INVOKE-DEBUGGER finds. If it is not, nothing has been changed.
;;;
;;; Measured: this still beats a hostile rebind (case A) and a hostile hook
;;; (case B), while leaving a bare SIGNAL alone (case C) and letting an inner
;;; HANDLER-CASE win (case D).
;;; ---------------------------------------------------------------------------

(defparameter *backtrace-frames* 12
  "How many stack frames to show when the process dies. 0 disables the backtrace.

A runtime condition has no compiler error context, so CLEF-CONDITIONS can give
the message but not a location -- and \"DIVISION-BY-ZERO signalled\" with no
indication of where is close to useless. The backtrace is the only thing that
answers 'where'.

Bounded rather than complete on purpose: SBCL's full backtrace through a deep
call chain is hundreds of frames, and the ones that matter are at the top. A
structured treatment via `dissect' is the eventual upgrade (roadmap W0), which
would let us drop the runner's own frames by name rather than by counting.")

(defparameter *noise-packages*
  '("SB-" "CLEF-RUNNER" "CLEF-CONDITIONS" "ASDF" "UIOP")
  "Package prefixes that mark a frame as machinery rather than the user's code.

SBCL's raw backtrace for a division by zero inside one user function runs to
nineteen frames, of which one is the user's. Leading with the other eighteen
buries the only line the reader wants.")

(defparameter *noise-functions*
  '("INVOKE-DEBUGGER" "ERROR" "SIGNAL" "LOAD" "COMPILE-FILE" "EVAL" "FUNCALL")
  "CL functions that are part of getting here rather than part of the program.

Matched exactly, not by prefix: a user function called ERROR-REPORTER is the
user's, and swallowing it would be the same class of mistake as the substring
matching this project removed from the language server.")

(defun frame-body (line)
  "A printed frame reads \"12: (FOO 1 2)\". Return the part after the number."
  (let ((colon (position #\: line)))
    (string-left-trim " " (if colon (subseq line (1+ colon)) line))))

(defparameter *anonymous-heads* '("LAMBDA" "FLET" "LABELS" "MACROLET")
  "Frame heads that name no function, so the frame must be judged by its :IN.")

(defun frame-head (body)
  "The name a frame should be judged by.

Not simply the first token. SBCL prints closures and local functions as

    ((LAMBDA NIL :IN SB-FASL::LOAD-AS-FASL))
    ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))

where the informative name is the enclosing one after :IN. Judging those by the
word LAMBDA keeps every closure in SBCL's loader, which is what the first version
of this filter did -- the user's three frames arrived buried under six of them."
  (let* ((tokens (remove "" (uiop:split-string (string-left-trim "(" body)
                                               :separator '(#\Space))
                         :test #'string=))
         (head (or (first tokens) ""))
         (name (cond
                 ((member head *anonymous-heads* :test #'string=)
                  (let ((tail (member ":IN" tokens :test #'string=)))
                    (or (second tail) head)))
                 ;; ((:METHOD FOO (T T)) ...) -- judge by FOO.
                 ((string= head ":METHOD") (or (second tokens) head))
                 (t head))))
    (string-right-trim ")" name)))

(defun noise-frame-p (line)
  (let ((head (frame-head (frame-body line))))
    (or (zerop (length head))
        ;; SBCL's marker for a form evaluated at load time. True, but it names
        ;; no code, so it cannot help anyone find anything.
        (char= (char head 0) #\")
        (some (lambda (p) (eql 0 (search p head))) *noise-packages*)
        (member head *noise-functions* :test #'string=))))

(defun print-trimmed-backtrace (stream)
  "Print the user's own frames from the top of the stack.

Says so plainly when nothing survives the filter, rather than printing an empty
heading or falling back to the raw dump. \"No frames of yours\" is real
information: it means the failure happened at load time or inside a library."
  (when (plusp *backtrace-frames*)
    (ignore-errors
     (let* ((text (with-output-to-string (s)
                    (sb-debug:print-backtrace
                     :stream s
                     ;; Generously more than we will show: the noise frames are
                     ;; interleaved, not clustered at the top.
                     :count (* 4 (max *backtrace-frames* 4))
                     :print-thread nil)))
            (lines (uiop:split-string (string-right-trim '(#\Newline) text)
                                      :separator '(#\Newline)))
            (interesting (remove-if #'noise-frame-p (remove "" lines :test #'string=))))
       (cond
         ((null interesting)
          (format stream "~&  (no frames from your code -- the failure is at load time~
                          ~%   or inside a library)~%"))
         (t
          (format stream "~&  in:~%")
          (loop for line in interesting
                repeat *backtrace-frames*
                do (format stream "    ~A~%" (frame-body line)))))))))

(defun report-unhandled (condition)
  "Render CONDITION as the reason the process is about to die."
  (let ((stream (diagnostic-stream)))
    (ignore-errors
     (clef-conditions:render (clef-conditions:extract condition) :stream stream))
    (print-trimmed-backtrace stream)
    (ignore-errors (finish-output stream))))

(defun clef-debugger-hook (condition hook)
  (declare (ignore hook))
  (report-unhandled condition)
  ;; :ABORT T so that unwinding through user code cannot signal again and turn a
  ;; clean failure into something else. Streams are flushed above.
  (sb-ext:exit :code +exit-failure+ :abort t))

(defun call-with-runtime (thunk)
  "Run THUNK under clef's process-level guarantees.

Establishes, in order of how hard they are to subvert:

  1. The debugger guarantee above -- the part that cannot be undone by load
     order, because it re-arms itself at signal time.
  2. Printer settings, so a condition mentioning a large structure prints a
     diagnostic rather than a megabyte. SBCL's defaults are unbounded.
  3. Silence from the compiler's progress chatter. Warnings still come through;
     these govern only the running commentary.

The optimize policy is NOT set here. A global proclamation would leak into
everything the image later loads, including code that declaimed its own; it
belongs to the compilation of a specific file. See COMPILE-WITH-POLICY."
  (let ((sb-ext:*invoke-debugger-hook* #'clef-debugger-hook)
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-length* 100)
        (*print-level* 12)
        (*print-circle* t)
        (*compile-verbose* nil)
        (*compile-print* nil)
        (*load-verbose* nil)
        (*load-print* nil))
    (handler-bind
        ((serious-condition
           (lambda (c)
             (declare (ignore c))
             (setf sb-ext:*invoke-debugger-hook* #'clef-debugger-hook)
             ;; Decline. See the block comment above -- this is the whole point.
             nil)))
      (funcall thunk))))

(defmacro with-runtime (&body body)
  `(call-with-runtime (lambda () ,@body)))
