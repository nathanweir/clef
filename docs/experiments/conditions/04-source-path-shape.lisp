;;;; What exactly is ORIGINAL-SOURCE-PATH, and can it be walked as a tree path?
;;;;
;;;; The survey established that FILE-POSITION names the enclosing top-level
;;;; form, not the error, and that ORIGINAL-SOURCE-PATH is what distinguishes
;;;; two errors inside one form. clef's language server currently uses only its
;;;; LAST element (the top-level form index) and then searches the form for the
;;;; symbol. To walk it as an actual path into the tree-sitter tree we need to
;;;; know:
;;;;
;;;;   1. Ordering -- innermost-first or outermost-first?
;;;;   2. Are the indices positional within the enclosing list, counting the
;;;;      operator as element 0?
;;;;   3. Do macroexpansions inject entries that have no counterpart in the
;;;;      source text, and can they be told apart?
;;;;
;;;; Errors are placed at deliberately asymmetric indices so the ordering cannot
;;;; be read two ways.
;;;;
;;;; Run: sbcl --script docs/experiments/conditions/04-source-path-shape.lisp

(require :uiop)

(defvar *report* (make-string-output-stream))
(defun say (fmt &rest args) (apply #'format *report* fmt args) (terpri *report*))

;;; Top-level form indices are 0-based over the forms as READ.
;;;   0 defpackage
;;;   1 in-package
;;;   2 padding-a
;;;   3 padding-b
;;;   4 padding-c
;;;   5 the form under test
;;;
;;; Inside form 5:  (defun target () (list 1 2 (no-such-fn) 4))
;;;   defun=0  target=1  ()=2  (list ...)=3
;;; Inside (list 1 2 (no-such-fn) 4):
;;;   list=0  1=1  2=2  (no-such-fn)=3  4=4
;;;
;;; So an outermost-first path is (5 3 3); innermost-first is (3 3 5).
;;; The top-level index 5 is large and unambiguous either way.
(defparameter *source* "
(defpackage :sp-probe (:use :cl))
(in-package :sp-probe)
(defun padding-a () 1)
(defun padding-b () 2)
(defun padding-c () 3)
(defun target ()
  (list 1 2 (no-such-fn) 4))
(defun two-in-one (unused-param)
  (list (no-such-a) 2 3 (no-such-b)))
(defmacro expands-to-bad ()
  '(no-such-from-macro))
(defun uses-macro ()
  (expands-to-bad))
")

(defun collect ()
  (let ((path (merge-pathnames "tmp/experiments/source-path.lisp" (truename ".")))
        (out '()))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string *source* s))
    (handler-bind
        ((condition
           (lambda (c)
             (let ((ctx (ignore-errors (sb-c::find-error-context nil))))
               (when ctx
                 (push (list :message (ignore-errors (princ-to-string c))
                             :file-position
                             (ignore-errors (sb-c::compiler-error-context-file-position ctx))
                             :source-path
                             (ignore-errors (sb-c::compiler-error-context-original-source-path ctx))
                             :original-source
                             (ignore-errors (sb-c::compiler-error-context-original-source ctx))
                             :context
                             (ignore-errors (sb-c::compiler-error-context-context ctx))
                             :enclosing
                             (ignore-errors (sb-c::compiler-error-context-enclosing-source ctx)))
                       out))))))
      (let ((*error-output* (make-broadcast-stream))
            (*standard-output* (make-broadcast-stream)))
        (ignore-errors
         (let ((fasl (compile-file path :verbose nil :print nil)))
           (when (and fasl (probe-file fasl)) (delete-file fasl))))))
    (nreverse out)))

;;; A reference implementation of "walk the path", run against the forms as we
;;; read them ourselves. If this lands on the right subform for every case, the
;;; same walk works against a tree-sitter tree.
(defun read-toplevel-forms (source)
  (with-input-from-string (in source)
    (let ((*package* *package*))
      (loop for form = (handler-case (read in nil :eof) (error () :eof))
            until (eq form :eof)
            collect form))))

(defun walk-path (forms path)
  "PATH is assumed outermost-first with the top-level index FIRST once reversed
appropriately; try both orders and report which one lands somewhere real."
  (labels ((descend (form indices)
             (cond ((null indices) form)
                   ((not (consp form)) (list :dead-end form indices))
                   ((>= (first indices) (length form))
                    (list :out-of-range form indices))
                   (t (descend (nth (first indices) form) (rest indices))))))
    (let* ((rev (reverse path))
           (top (first rev)))
      (if (and (integerp top) (< top (length forms)))
          (descend (nth top forms) (rest rev))
          :no-toplevel))))

(let ((results (collect))
      (forms (let ((*package* (find-package :cl-user)))
               (ignore-errors (read-toplevel-forms *source*)))))
  (say "~&read ~A top-level forms~%" (length forms))
  (dolist (r results)
    (say "~&---")
    (say "  message:        ~A"
         (substitute #\Space #\Newline (or (getf r :message) "")))
    (say "  file-position:  ~S" (getf r :file-position))
    (say "  source-path:    ~S" (getf r :source-path))
    (say "  original-source:~S" (getf r :original-source))
    (say "  context:        ~S" (getf r :context))
    (say "  enclosing:      ~S" (getf r :enclosing))
    (say "  walk(reversed): ~S" (ignore-errors (walk-path forms (getf r :source-path))))))

(format t "~&~A~%" (get-output-stream-string *report*))
