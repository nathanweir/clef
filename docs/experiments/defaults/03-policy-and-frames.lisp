;;;; Does WITH-COMPILATION-UNIT :POLICY actually apply, and does (debug 3) keep
;;;; user frames on the stack?
;;;;
;;;; The runner claims to compile under (debug 3) (safety 3) so that failures are
;;;; legible. That claim is worth nothing if the keyword is ignored -- and the
;;;; first backtrace the runner produced was missing the user's own functions,
;;;; which is exactly what insufficient debug settings look like.
;;;;
;;;; TWO TRAPS, both of which this probe fell into on its first draft:
;;;;
;;;;   1. Wrapping the call in IGNORE-ERRORS *inside* a HANDLER-BIND. IGNORE-ERRORS
;;;;      is HANDLER-CASE, which unwinds immediately -- so the outer handler never
;;;;      runs and the backtrace is never captured. Every case then reports zero
;;;;      frames and it looks like a compiler setting. HANDLER-CASE goes OUTSIDE.
;;;;
;;;;   2. Reading SB-C::*POLICY* from a function at runtime. That is the *global*
;;;;      policy, not the one the file was compiled under, so a dynamically-scoped
;;;;      :POLICY always looks like it did nothing. The observable consequence --
;;;;      whether frames survive -- is the honest measurement.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/defaults/03-policy-and-frames.lisp

(require :uiop)

(defvar *report* (make-string-output-stream))
(defun say (fmt &rest args) (apply #'format *report* fmt args) (terpri *report*))

;;; (debug 3) suppresses tail-call merging, so MIDDLE and OUTER should survive as
;;; distinct frames. At the default policy they are merged away.
(defparameter *source* "(defpackage :policy-probe (:use :cl))
(in-package :policy-probe)
(defun inner (x) (/ 10 x))
(defun middle (x) (inner x))
(defun outer (x) (middle x))
")

(defun write-source (n)
  (let ((path (merge-pathnames (format nil "tmp/experiments/policy-probe-~A.lisp" n)
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string *source* s))
    path))

(defun user-frames (label)
  "Provoke a failure in the loaded probe code and count its own frames."
  (let ((frames '()))
    ;; HANDLER-CASE OUTSIDE, HANDLER-BIND INSIDE. See trap 1 above.
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
          (funcall (find-symbol "OUTER" :policy-probe) 0))
      (error () nil))
    (let ((user (remove-if-not (lambda (l) (search "POLICY-PROBE" l)) frames)))
      (say "~&~%=== ~A ===" label)
      (say "  total frames captured: ~A" (length frames))
      (say "  user frames visible:   ~A" (length user))
      (dolist (f user) (say "    ~A" (string-left-trim " " f)))
      (length user))))

(defun compile-load (path &optional policy)
  (let ((*error-output* (make-broadcast-stream))
        (*standard-output* (make-broadcast-stream)))
    (if policy
        (with-compilation-unit (:policy policy)
          (load (compile-file path :verbose nil :print nil)))
        (load (compile-file path :verbose nil :print nil)))))

;;; 1. SBCL's defaults.
(compile-load (write-source 1))
(user-frames "default policy (1/1/1)")

;;; 2. :POLICY given the shape the runner used -- a LIST containing the
;;;    declaration, i.e. '((optimize ...)).
(delete-package :policy-probe)
(compile-load (write-source 2) '((optimize (debug 3) (safety 3) (speed 1))))
(user-frames "with-compilation-unit :policy '((optimize ...))")

;;; 3. :POLICY given the bare declaration, '(optimize ...).
(delete-package :policy-probe)
(compile-load (write-source 3) '(optimize (debug 3) (safety 3) (speed 1)))
(user-frames "with-compilation-unit :policy '(optimize ...)")

;;; 4. Control: a global PROCLAIM, which definitely works.
(delete-package :policy-probe)
(proclaim '(optimize (debug 3) (safety 3) (speed 1)))
(compile-load (write-source 4))
(user-frames "global proclaim (3/3/1)")

(format t "~&~A~%" (get-output-stream-string *report*))
