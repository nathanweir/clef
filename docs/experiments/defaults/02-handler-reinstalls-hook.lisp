;;;; Can the runner keep the guarantee WITHOUT breaking SIGNAL semantics?
;;;;
;;;; Probe 01 established the design constraint: --disable-debugger is only a
;;;; value in *INVOKE-DEBUGGER-HOOK*, so hostile code can rebind it and reach the
;;;; debugger anyway (cases 5 and 6), while an outer HANDLER-BIND survives both
;;;; (cases 8 and 9).
;;;;
;;;; But "outer HANDLER-BIND that renders and exits" over-reaches. HANDLER-BIND
;;;; handlers run for every SIGNAL, not only for calls that will reach the
;;;; debugger. Code that does
;;;;
;;;;     (signal (make-condition 'simple-error :format-control "advisory"))
;;;;
;;;; is entitled to have that return NIL and carry on. A runner that exits there
;;;; kills correct programs.
;;;;
;;;; The proposed fix: the outer handler does not exit. It RE-INSTALLS the
;;;; debugger hook and then DECLINES. Because it runs during the signal, it is
;;;; already inside the extent of any hostile LET binding, so its SETF lands on
;;;; that binding. If the condition really is heading for the debugger, our
;;;; freshly installed hook is what INVOKE-DEBUGGER finds. If it is not, we have
;;;; changed nothing.
;;;;
;;;; Three things to verify:
;;;;   A. It still wins against a hostile rebind (probe 01 case 6 must stay fixed).
;;;;   B. It still wins against a hostile hook (probe 01 case 9).
;;;;   C. A bare SIGNAL of a SERIOUS-CONDITION is left alone -- the case the
;;;;      exit-immediately design would have broken.
;;;;   D. An inner HANDLER-CASE still wins, i.e. we do not steal handled errors.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/defaults/02-handler-reinstalls-hook.lisp

(require :sb-posix)

(defparameter *sbcl* "sbcl")
(defparameter *base-flags* '("--noinform" "--no-userinit" "--no-sysinit"))

;;; The mechanism under test, as a string so each probe gets a fresh image.
(defparameter *runtime* "
(defun clef-hook (condition hook)
  (declare (ignore hook))
  (format *error-output* \"~&CLEF-HOOK: ~A~%\" condition)
  (finish-output *error-output*)
  (sb-ext:exit :code 9 :abort t))

(defmacro with-clef-runtime (&body body)
  `(let ((sb-ext:*invoke-debugger-hook* #'clef-hook))
     (handler-bind
         ((serious-condition
            (lambda (c)
              (declare (ignore c))
              ;; Re-install, then DECLINE. Running here means we are inside
              ;; whatever binding is currently in effect, hostile or not.
              (setf sb-ext:*invoke-debugger-hook* #'clef-hook)
              nil)))
       ,@body)))
")

;;; --eval reads exactly ONE form, so the three-form runtime above has to reach
;;; the child through --load. Passing it to --eval silently defines only the
;;; DEFUN and leaves WITH-CLEF-RUNTIME undefined, which makes every probe fail
;;; identically and look like the mechanism is broken.
(defparameter *runtime-path*
  (let ((path (merge-pathnames "tmp/experiments/clef-runtime-probe.lisp"
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string *runtime* s))
    path))

(defun run-probe (label form)
  (let* ((args (append *base-flags*
                       (list "--load" (namestring *runtime-path*)
                             "--eval" form
                             "--eval" "(sb-ext:exit :code 0)")))
         (proc (sb-ext:run-program *sbcl* args
                                   :search t :input nil
                                   :output :stream :error :stream :wait nil)))
    (let ((exited (loop repeat 200
                        for status = (sb-ext:process-status proc)
                        when (not (eq status :running)) return t
                        do (sleep 0.05)
                        finally (return nil))))
      (unless exited
        (sb-ext:process-kill proc 9)
        (sb-ext:process-wait proc))
      (let ((out (with-output-to-string (s)
                   (dolist (stream (list (sb-ext:process-output proc)
                                         (sb-ext:process-error proc)))
                     (loop for line = (read-line stream nil nil)
                           while line do (write-line line s))))))
        (format t "~&~%=== ~A ===~%" label)
        (format t "  exited: ~A   code: ~A~%"
                (if exited "yes" "NO (killed)") (sb-ext:process-exit-code proc))
        (dolist (line (with-input-from-string (s out)
                        (loop for l = (read-line s nil nil) while l
                              when (or (search "CLEF-" l) (search "PROBE-" l)
                                       (search "debugger invoked" l)
                                       (search "unhandled" l))
                                collect l)))
          (format t "    | ~A~%" line))))))

;;; A. Hostile rebind of the hook, then a real error.
(run-probe "A. hostile rebind, then ERROR"
           "(with-clef-runtime
              (let ((sb-ext:*invoke-debugger-hook* nil))
                (error \"PROBE-rebound\")))")

;;; B. Hostile code installs its OWN hook.
(run-probe "B. hostile hook, then ERROR"
           "(with-clef-runtime
              (let ((sb-ext:*invoke-debugger-hook*
                      (lambda (c h) (declare (ignore c h))
                        (format *error-output* \"~&PROBE-hostile-hook ran~%\")
                        (sb-ext:exit :code 3 :abort t))))
                (error \"PROBE-hostile\")))")

;;; C. THE REGRESSION GUARD. A bare SIGNAL of a serious condition must be
;;;    declined and execution must continue. Exit code 0, and PROBE-survived
;;;    must be printed.
(run-probe "C. bare SIGNAL of a serious condition is left alone"
           "(with-clef-runtime
              (signal (make-condition 'simple-error :format-control \"PROBE-advisory\"))
              (format *error-output* \"~&PROBE-survived~%\"))")

;;; D. An inner HANDLER-CASE must still win; we must not steal handled errors.
(run-probe "D. inner HANDLER-CASE still wins"
           "(with-clef-runtime
              (handler-case (error \"PROBE-inner\")
                (error (c) (format *error-output* \"~&PROBE-handled: ~A~%\" c))))")

;;; E. Control: no hostility at all, plain unhandled error.
(run-probe "E. plain unhandled ERROR"
           "(with-clef-runtime (error \"PROBE-plain\"))")

(format t "~&~%done~%")
