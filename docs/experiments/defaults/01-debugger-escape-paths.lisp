;;;; Can a non-interactive toolchain actually be pinned, or does something
;;;; always drop you into the debugger anyway?
;;;;
;;;; The standing disagreement (motivation.md 5.1): "just set a flag" is the
;;;; usual answer, and it has repeatedly failed in practice. The claim to test is
;;;; that making YOUR program non-interactive is easy, but making the whole
;;;; toolchain non-interactive is not, because any layer can signal into its own
;;;; restart.
;;;;
;;;; Run WITHOUT --disable-debugger so we are testing the hooks, not the flag:
;;;;   sbcl --noinform --non-interactive --load 01-debugger-escape-paths.lisp
;;;;
;;;; --non-interactive implies --disable-debugger in SBCL, so the runner below
;;;; probes each path in a subprocess with an explicit configuration instead.

(require :sb-posix)

(defparameter *sbcl* "sbcl")

;; --no-userinit/--no-sysinit on every probe: we are testing SBCL's own
;; behaviour, not this machine's ~/.sbclrc.
(defparameter *base-flags* '("--noinform" "--no-userinit" "--no-sysinit"))

(defun run-probe (label setup form &key (flags *base-flags*))
  "Run FORM in a fresh SBCL with SETUP applied. Report whether it hung,
exited cleanly, or escaped into something interactive."
  (let* ((args (append flags
                       (when setup (list "--eval" setup))
                       (list "--eval" form
                             "--eval" "(sb-ext:exit :code 0)")))
         (proc (sb-ext:run-program *sbcl* args
                                   :search t
                                   :input nil          ; EOF immediately
                                   :output :stream
                                   :error :stream
                                   :wait nil)))
    ;; A process that drops into a REPL will sit waiting on stdin. With :input
    ;; nil it gets EOF instead, so the tell is the exit code and what it printed.
    (let ((exited (loop repeat 200
                        for status = (sb-ext:process-status proc)
                        when (not (eq status :running)) return t
                        do (sleep 0.05)
                        finally (return nil))))
      (unless exited
        (sb-ext:process-kill proc 9)
        (sb-ext:process-wait proc))
      (let* ((err (with-output-to-string (s)
                    (let ((stream (sb-ext:process-error proc)))
                      (loop for line = (read-line stream nil nil)
                            while line do (write-line line s)))))
             (code (sb-ext:process-exit-code proc)))
        (format t "~&~%=== ~A ===~%" label)
        (format t "  exited: ~A   code: ~A~%" (if exited "yes" "NO (killed)") code)
        (let ((interesting
                (remove-if-not
                 (lambda (l) (or (search "debugger" l :test #'char-equal)
                                 (search "restart" l :test #'char-equal)
                                 (search "Ctrl" l :test #'char-equal)
                                 (search "unhandled" l :test #'char-equal)
                                 (search "PROBE-" l)))
                 (uiop:split-string err :separator '(#\Newline)))))
          (dolist (l (remove "" interesting :test #'string=))
            (format t "    | ~A~%" l)))
        (values exited code)))))

(format t "~&SBCL ~A~%" (lisp-implementation-version))

;;; Path 1: an ordinary error in your own code, nothing configured.
(run-probe "1. plain error, no configuration"
           nil
           "(error \"PROBE-plain\")")

;;; Path 2: the same, with the documented hook set. This is the "just set a
;;; flag" answer in its strongest form.
(run-probe "2. plain error, *invoke-debugger-hook* set"
           "(setf sb-ext:*invoke-debugger-hook*
                  (lambda (c h) (declare (ignore h))
                    (format *error-output* \"PROBE-hook caught: ~A~%\" c)
                    (sb-ext:exit :code 7 :abort t)))"
           "(error \"PROBE-hooked\")")

;;; Path 3: --disable-debugger, the blunt instrument.
(run-probe "3. plain error, --disable-debugger"
           nil
           "(error \"PROBE-disabled\")"
           :flags (append *base-flags* (list "--disable-debugger")))

;;; Path 4: does the hook survive a handler-bind that declines to handle?
(run-probe "4. hook + intervening handler-bind that declines"
           "(setf sb-ext:*invoke-debugger-hook*
                  (lambda (c h) (declare (ignore h))
                    (format *error-output* \"PROBE-hook caught: ~A~%\" c)
                    (sb-ext:exit :code 7 :abort t)))"
           "(handler-bind ((warning #'muffle-warning)) (error \"PROBE-nested\"))")

;;; Path 5: THE INTERESTING ONE. A library that binds the hook itself, the way
;;; a REPL, test framework or contrib can. Does our configuration survive code
;;; we do not control?
(run-probe "5. hook set, then REBOUND by 'library' code"
           "(setf sb-ext:*invoke-debugger-hook*
                  (lambda (c h) (declare (ignore h))
                    (format *error-output* \"PROBE-hook caught: ~A~%\" c)
                    (sb-ext:exit :code 7 :abort t)))"
           "(let ((sb-ext:*invoke-debugger-hook* nil)) (error \"PROBE-rebound\"))")

;;; Path 6: --disable-debugger, then library code rebinds the hook away.
;;; If the flag is genuinely a floor, this still must not go interactive.
(run-probe "6. --disable-debugger, then hook rebound to nil"
           nil
           "(let ((sb-ext:*invoke-debugger-hook* nil)) (error \"PROBE-rebound-flag\"))"
           :flags (append *base-flags* (list "--disable-debugger")))

;;; Path 7: a failure during ASDF loading rather than in user code.
(run-probe "7. error inside an ASDF operation, --disable-debugger"
           "(require :asdf)"
           "(asdf:load-system :no-such-system-probe)"
           :flags (append *base-flags* (list "--disable-debugger")))

;;; Path 8: if the hook is defeatable by rebinding, is an outer HANDLER-BIND?
;;; A handler runs before the debugger is ever reached, so hostile rebinding of
;;; *invoke-debugger-hook* should be irrelevant. If this holds, it -- not the
;;; flag -- is the mechanism the golden path should be built on.
(run-probe "8. outer handler-bind, library rebinds hook to nil"
           nil
           "(handler-bind ((serious-condition
                             (lambda (c)
                               (format *error-output* \"PROBE-handler caught: ~A~%\" c)
                               (sb-ext:exit :code 9 :abort t))))
              (let ((sb-ext:*invoke-debugger-hook* nil))
                (error \"PROBE-handler-vs-rebind\")))")

;;; Path 9: same, but the hostile code also installs its own debugger hook
;;; rather than merely clearing ours.
(run-probe "9. outer handler-bind, library installs its OWN hook"
           nil
           "(handler-bind ((serious-condition
                             (lambda (c)
                               (format *error-output* \"PROBE-handler caught: ~A~%\" c)
                               (sb-ext:exit :code 9 :abort t))))
              (let ((sb-ext:*invoke-debugger-hook*
                      (lambda (c h) (declare (ignore c h))
                        (format *error-output* \"PROBE-hostile hook ran~%\"))))
                (error \"PROBE-handler-vs-hostile\")))")

(format t "~&~%done~%")
