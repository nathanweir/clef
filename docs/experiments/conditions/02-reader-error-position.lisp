;;;; Does a reader error carry its position as structure, or only as text?
;;;;
;;;; The W0 survey claimed reader errors carry no usable position, and clef's
;;;; diagnostics fall back to tree-sitter for them. But the printed message of a
;;;; COMPILER-ERROR for a bad package prefix contains
;;;;
;;;;     Line: 2, Column: 32, File-Position: 45
;;;;
;;;; which is far too precise to be a guess. This probe asks whether those three
;;;; numbers are reachable as data, or whether SBCL computes them only while
;;;; rendering the report.
;;;;
;;;; Run: sbcl --script docs/experiments/conditions/02-reader-error-position.lisp

(require :uiop)

(defvar *report* (make-string-output-stream))

(defun say (fmt &rest args)
  (apply #'format *report* fmt args)
  (terpri *report*))

(defun describe-condition (label c)
  (say "~&--- ~A" label)
  (say "  class:        ~S" (type-of c))
  (say "  precedence:   ~S"
       (mapcar #'class-name
               (sb-mop:class-precedence-list (class-of c))))
  (say "  error-p:      ~S" (typep c 'error))
  (say "  encapsulated: ~S" (typep c 'sb-int:encapsulated-condition))
  ;; Every bound slot, by name. We are looking for anything holding a number.
  (dolist (slot (sb-mop:class-slots (class-of c)))
    (let ((name (sb-mop:slot-definition-name slot)))
      (say "  slot ~30S ~S"
           name
           (if (slot-boundp c name)
               (let ((v (slot-value c name)))
                 (if (stringp v) v (ignore-errors (princ-to-string v))))
               :unbound))))
  (say "  report:       ~S" (ignore-errors (princ-to-string c))))

;;; ---------------------------------------------------------------------------
;;; 1. A reader error signalled directly, outside the compiler.
;;; ---------------------------------------------------------------------------

(defun probe-direct-read ()
  (let ((path (merge-pathnames "tmp/experiments/reader-direct.lisp"
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(defun ok () 1)
(defun bad () (no-such-pkg-xyz:g 1))
" s))
    (with-open-file (in path)
      (handler-case
          (loop (let ((form (read in nil :eof)))
                  (when (eq form :eof) (return))))
        (condition (c)
          (describe-condition "direct READ of a bad package prefix" c)
          ;; SBCL exposes a position-info helper. Does it work here?
          (say "  stream-error-stream:   ~S"
               (ignore-errors (stream-error-stream c)))
          (say "  position-info:         ~S"
               (ignore-errors
                (sb-impl::stream-error-position-info (stream-error-stream c))))
          (say "  file-position of stream: ~S"
               (ignore-errors (file-position (stream-error-stream c)))))))))

;;; ---------------------------------------------------------------------------
;;; 2. The same error, seen through COMPILE-FILE -- which is how clef sees it.
;;; ---------------------------------------------------------------------------

(defun probe-compiled-read ()
  (let ((path (merge-pathnames "tmp/experiments/reader-compiled.lisp"
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(defun ok () 1)
(defun bad () (no-such-pkg-xyz:g 1))
" s))
    (let ((seen '()))
      (handler-bind ((condition (lambda (c) (push c seen))))
        (let ((*error-output* (make-broadcast-stream))
              (*standard-output* (make-broadcast-stream)))
          (ignore-errors
           (let ((fasl (compile-file path :verbose nil :print nil)))
             (when (and fasl (probe-file fasl)) (delete-file fasl))))))
      (say "~&~%=== through COMPILE-FILE: ~A condition(s) ===" (length seen))
      (dolist (c (nreverse seen))
        (describe-condition (format nil "compiled: ~S" (type-of c)) c)
        ;; The interesting question: unwrap it and ask the inner condition.
        (when (typep c 'sb-int:encapsulated-condition)
          (let ((inner (ignore-errors (sb-int:encapsulated-condition c))))
            (when inner
              (describe-condition "  ...unwrapped" inner)
              (say "  inner stream:        ~S"
                   (ignore-errors (stream-error-stream inner)))
              (say "  inner position-info: ~S"
                   (ignore-errors
                    (sb-impl::stream-error-position-info
                     (stream-error-stream inner)))))))
        ;; And does the compiler error context know anything?
        (say "  find-error-context:  ~S"
             (ignore-errors (sb-c::find-error-context nil)))))))

;;; ---------------------------------------------------------------------------
;;; 3. An unbalanced form -- END-OF-FILE, the case the survey called hopeless.
;;; ---------------------------------------------------------------------------

(defun probe-truncated ()
  (let ((path (merge-pathnames "tmp/experiments/reader-truncated.lisp"
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(defun ok () 1)
(defun truncated () (+ 1 2)
" s))
    (let ((seen '()))
      (handler-bind ((condition (lambda (c) (push c seen))))
        (let ((*error-output* (make-broadcast-stream))
              (*standard-output* (make-broadcast-stream)))
          (ignore-errors
           (let ((fasl (compile-file path :verbose nil :print nil)))
             (when (and fasl (probe-file fasl)) (delete-file fasl))))))
      (say "~&~%=== truncated form: ~A condition(s) ===" (length seen))
      (dolist (c (nreverse seen))
        (describe-condition (format nil "truncated: ~S" (type-of c)) c)
        (when (typep c 'sb-int:encapsulated-condition)
          (let ((inner (ignore-errors (sb-int:encapsulated-condition c))))
            (when inner
              (describe-condition "  ...unwrapped" inner)
              (say "  inner position-info: ~S"
                   (ignore-errors
                    (sb-impl::stream-error-position-info
                     (stream-error-stream inner)))))))))))

(probe-direct-read)
(probe-compiled-read)
(probe-truncated)

;;; Capture first, print second. Printing into a stream you have just rebound to
;;; a broadcast sink swallows the output and reads as "nothing was signalled".
(format t "~&~A~%" (get-output-stream-string *report*))
