;;;; Follow-up to 02: pin down exactly which accessors to build on.
;;;;
;;;; 02 established that reader errors DO carry structured position, contrary to
;;;; the W0 survey. This probe answers the three questions that decide the API:
;;;;
;;;;   1. How many layers of encapsulation are there, and does UNWRAP need to
;;;;      loop rather than peel one?
;;;;   2. Which position source is authoritative for which failure -- the
;;;;      INPUT-ERROR-IN-COMPILE-FILE slots, or the stream's position info?
;;;;   3. Can we recover the file path, and a message free of the
;;;;      "Stream: #<FORM-TRACKING-STREAM ...>" trailer?
;;;;
;;;; Run: sbcl --script docs/experiments/conditions/03-reader-error-api.lisp

(require :uiop)

(defvar *report* (make-string-output-stream))
(defun say (fmt &rest args) (apply #'format *report* fmt args) (terpri *report*))

(defun unwrap-fully (c)
  "Peel every layer of encapsulation, defensively bounded."
  (loop repeat 10
        while (typep c 'sb-int:encapsulated-condition)
        do (let ((inner (ignore-errors (sb-int:encapsulated-condition c))))
             (if (and inner (not (eq inner c))) (setf c inner) (return))))
  c)

(defun probe (label source)
  (let ((path (merge-pathnames (format nil "tmp/experiments/api-~A.lisp" label)
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string source s))
    (let ((seen '()))
      (handler-bind ((condition (lambda (c) (push c seen))))
        (let ((*error-output* (make-broadcast-stream))
              (*standard-output* (make-broadcast-stream)))
          (ignore-errors
           (let ((fasl (compile-file path :verbose nil :print nil)))
             (when (and fasl (probe-file fasl)) (delete-file fasl))))))
      (say "~&~%=== ~A ===" label)
      (say "  source: ~S" source)
      (dolist (c (nreverse seen))
        (let* ((chain (loop for x = c then (sb-int:encapsulated-condition x)
                            collect (type-of x)
                            while (typep x 'sb-int:encapsulated-condition)
                            repeat 10))
               (inner (unwrap-fully c))
               ;; The middle layer is the one carrying compile-file's own idea
               ;; of position; find it wherever it sits in the chain.
               (wrapper (loop for x = c then (sb-int:encapsulated-condition x)
                              when (typep x 'sb-c::input-error-in-compile-file)
                                return x
                              while (typep x 'sb-int:encapsulated-condition)
                              repeat 10)))
          (say "  chain:            ~S" chain)
          (say "  innermost:        ~S" (type-of inner))
          (say "  innermost error-p:~S" (typep inner 'error))
          (say "  simple-condition: ~S" (typep inner 'simple-condition))
          (when (typep inner 'simple-condition)
            (say "  format-control:   ~S"
                 (ignore-errors (simple-condition-format-control inner)))
            (say "  format-arguments: ~S"
                 (ignore-errors (simple-condition-format-arguments inner)))
            (say "  clean message:    ~S"
                 (ignore-errors
                  (apply #'format nil
                         (simple-condition-format-control inner)
                         (simple-condition-format-arguments inner)))))
          (when wrapper
            (say "  wrapper POSITION: ~S" (ignore-errors (slot-value wrapper 'sb-c::position)))
            (say "  wrapper LINE/COL: ~S" (ignore-errors (slot-value wrapper 'sb-c::line/col))))
          (let ((stream (ignore-errors (stream-error-stream inner))))
            (say "  stream:           ~S" (type-of stream))
            (say "  stream pathname:  ~S" (ignore-errors (pathname stream)))
            (say "  position-info:    ~S"
                 (ignore-errors (sb-impl::stream-error-position-info stream)))
            ;; FORM-TRACKING-STREAM knows where the current top-level form began.
            (say "  form-start-char:  ~S"
                 (ignore-errors (sb-int:form-tracking-stream-form-start-char-pos stream)))
            (say "  form-start-byte:  ~S"
                 (ignore-errors (sb-int:form-tracking-stream-form-start-byte-pos stream)))))))))

(probe "package-prefix" "(defun ok () 1)
(defun bad () (no-such-pkg-xyz:g 1))
")

(probe "truncated" "(defun ok () 1)
(defun truncated () (+ 1 2)
")

(probe "unbalanced-close" "(defun ok () 1)
(defun extra () 1))
(defun after () 2)
")

(probe "bad-token" "(defun ok () 1)
(defun bad () #\\)
")

(format t "~&~A~%" (get-output-stream-string *report*))
