;;;; Does the runner's OWN compile path get the policy it claims?
;;;;
;;;; Probe 03 showed that :POLICY '(optimize ...) works and '((optimize ...))
;;;; silently does not. The runner was fixed to use the working shape -- and the
;;;; user's frames are STILL missing from its backtraces. So either the fix did
;;;; not take, or something else about the runner's path differs from the probe.
;;;;
;;;; Test the real function rather than a reconstruction of it.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/defaults/04-runner-policy-check.lisp

(require :asdf)
(defparameter *repo* (make-pathname :directory (butlast (pathname-directory *load-truename*) 3)))
(asdf:initialize-output-translations
 `(:output-translations ((,*repo* :**/ :*.*.*) (,*repo* "build" :**/ :*.*.*))
                        :inherit-configuration))
(asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *repo*))
(asdf:load-asd (merge-pathnames "runner/clef-runner.asd" *repo*))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :clef-runner :verbose nil))

(defparameter *source* "(defpackage :rp (:use :cl))
(in-package :rp)
(eval-when (:compile-toplevel)
  (format *trace-output* \"~&POLICY-AT-COMPILE-TIME: debug=~A safety=~A speed=~A~%\"
          (sb-c::policy-quality sb-c::*policy* 'debug)
          (sb-c::policy-quality sb-c::*policy* 'safety)
          (sb-c::policy-quality sb-c::*policy* 'speed)))
(defun inner (x) (/ 10 x))
(defun middle (x) (inner x))
(defun outer (x) (middle x))
")

(defun write-source (n)
  (let ((path (merge-pathnames (format nil "tmp/experiments/runner-policy-~A.lisp" n)
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string *source* s))
    path))

(defun user-frames (label)
  (let ((frames '()))
    ;; HANDLER-CASE outside, HANDLER-BIND inside -- see probe 03's trap 1.
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
          (funcall (find-symbol "OUTER" :rp) 0))
      (error () nil))
    (let ((user (remove-if-not (lambda (l) (search "RP::" l)) frames)))
      (format t "~&  ~A -> ~A user frame(s)~%" label (length user))
      (dolist (f user) (format t "      ~A~%" (string-left-trim " " f))))))

;;; The runner's own function, at its default :DEV policy.
(format t "~&=== clef-runner::compile-and-load, *optimize-policy* = :dev ===~%")
(format t "~&  optimize-declaration says: ~S~%"
        (clef-runner::optimize-declaration :dev))
(let ((clef-runner::*optimize-policy* :dev))
  (clef-runner::compile-and-load (write-source 1)))
(user-frames "runner :dev")

;;; And the release policy, which should NOT keep the frames.
(delete-package :rp)
(format t "~&~%=== clef-runner::compile-and-load, *optimize-policy* = :release ===~%")
(let ((clef-runner::*optimize-policy* :release))
  (clef-runner::compile-and-load (write-source 2)))
(user-frames "runner :release")
