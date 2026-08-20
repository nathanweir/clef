;;;; How many conditions does SBCL signal for one undefined name used N times?
;;;;
;;;; This decides a real UX question. clef reports every occurrence of an
;;;; undefined symbol, and a test pins that at 3-for-3. Walking
;;;; ORIGINAL-SOURCE-PATH to the exact subform dropped it to 1, which means SBCL
;;;; is NOT signalling once per call site. Before choosing between "mark the one
;;;; site SBCL named" and "mark every use", find out what SBCL is actually
;;;; telling us -- in particular whether the warning is scoped to a form, to a
;;;; file, or to the whole compilation unit.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/conditions/07-undefined-grouping.lisp

(require :uiop)

(defvar *report* (make-string-output-stream))
(defun say (fmt &rest args) (apply #'format *report* fmt args) (terpri *report*))

(defun probe (label source)
  (let ((path (merge-pathnames (format nil "tmp/experiments/ug-~A.lisp" label)
                               (truename ".")))
        (seen '()))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string source s))
    (handler-bind
        ((condition
           (lambda (c)
             (when (typep c '(or warning error))
               (push (list :type (type-of c)
                           :message (ignore-errors (princ-to-string c))
                           :source-path
                           (let ((ctx (ignore-errors (sb-c::find-error-context nil))))
                             (when ctx
                               (ignore-errors
                                (sb-c::compiler-error-context-original-source-path ctx))))
                           :original-source
                           (let ((ctx (ignore-errors (sb-c::find-error-context nil))))
                             (when ctx
                               (ignore-errors
                                (sb-c::compiler-error-context-original-source ctx))))
                           :context
                           (let ((ctx (ignore-errors (sb-c::find-error-context nil))))
                             (when ctx
                               (ignore-errors
                                (sb-c::compiler-error-context-context ctx)))))
                     seen)))))
      (let ((*error-output* (make-broadcast-stream))
            (*standard-output* (make-broadcast-stream)))
        (ignore-errors
         (let ((fasl (compile-file path :verbose nil :print nil)))
           (when (and fasl (probe-file fasl)) (delete-file fasl))))))
    (say "~&~%========== ~A: ~A condition(s) ==========" label (length seen))
    (say "~A" source)
    (dolist (r (nreverse seen))
      (say "  ~S" (getf r :type))
      (say "    message: ~A" (substitute #\Space #\Newline (or (getf r :message) "")))
      (say "    path:    ~S" (getf r :source-path))
      (say "    source:  ~A"
           (substitute #\Space #\Newline (princ-to-string (or (getf r :original-source) ""))))
      (say "    context: ~S" (getf r :context)))))

;;; Three uses of one undefined name, all in one defun.
(probe "three-in-one-defun" "(defun foo ()
  (undefined-xyz 1)
  (undefined-xyz 2)
  (undefined-xyz 3))
")

;;; Three uses of one undefined name, spread across three defuns.
(probe "three-across-defuns" "(defun a () (undefined-xyz 1))
(defun b () (undefined-xyz 2))
(defun c () (undefined-xyz 3))
")

;;; Two different undefined names, to confirm grouping is by name.
(probe "two-names" "(defun foo ()
  (undefined-aaa 1)
  (undefined-bbb 2)
  (undefined-aaa 3))
")

;;; An undefined VARIABLE used three times -- does it group the same way?
(probe "undefined-variable-thrice" "(defun foo ()
  (list undefined-var undefined-var undefined-var))
")

;;; A wrong-arity call made twice, for contrast: is this per-site?
(probe "wrong-arity-twice" "(defun two (a b) (list a b))
(defun caller ()
  (list (two 1 2 3) (two 1 2 3 4)))
")

(format t "~&~A~%" (get-output-stream-string *report*))
