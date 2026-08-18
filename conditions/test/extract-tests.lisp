(in-package :clef-conditions/test)

;;;; Tests for structured condition extraction.
;;;;
;;;; These compile real source through SBCL and assert on what comes back, so
;;;; they double as the canary for the SB-C internals this depends on: if an
;;;; SBCL upgrade moves the compiler error context, these fail loudly rather
;;;; than silently degrading to "no location".

(defvar *failures* '())
(defvar *checks* 0)

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (if (funcall test got expected)
      (format t "  ~C[32m✓~C[0m ~A~%" #\Escape #\Escape label)
      (progn
        (push (format nil "~A: expected ~S, got ~S" label expected got) *failures*)
        (format t "  ~C[31m✗~C[0m ~A: expected ~S, got ~S~%"
                #\Escape #\Escape label expected got))))

(defun check-true (label got)
  (check label (and got t) t))

(defparameter *source* "
(defpackage :clef-cond-test-pkg (:use :cl))
(in-package :clef-cond-test-pkg)

(defun calls-undefined ()
  (no-such-function 1 2))

(defun reads-undefined ()
  (+ 1 no-such-variable))

(defun unused-binding ()
  (let ((never-used 5))
    42))

(defun wrong-arity ()
  (calls-undefined 1 2 3))
")

(defun collect-diagnostics (source)
  "Compile SOURCE and return every DIAGNOSTIC extracted along the way."
  (let ((out '()))
    (uiop:call-with-temporary-file
     (lambda (stream path)
       (write-string source stream)
       (force-output stream)
       (close stream)
       (handler-bind ((condition
                        (lambda (c)
                          (when (typep c '(or warning error sb-c:compiler-error))
                            (push (clef-conditions:extract c) out)))))
         (let ((*error-output* (make-broadcast-stream))
               (*standard-output* (make-broadcast-stream)))
           (ignore-errors
            (let ((fasl (compile-file path :verbose nil :print nil)))
              (when (and fasl (probe-file fasl)) (delete-file fasl)))))))
     :want-stream-p t :want-pathname-p t :type "lisp" :keep nil)
    (nreverse out)))

(defun find-by-kind (diags kind)
  (find kind diags :key #'clef-conditions:diagnostic-kind))

(defun run-all-tests ()
  (setf *failures* '() *checks* 0)
  (format t "~&Running clef-conditions tests~%~%")
  (let ((diags (collect-diagnostics *source*)))

    (format t "extraction~%")
    (check-true "produced diagnostics" (plusp (length diags)))

    ;; Every diagnostic must carry a location. This is the whole point: the
    ;; previous approach searched source text because it had none.
    (format t "~&location~%")
    (check "every diagnostic has a file position"
           (count-if #'null diags :key #'clef-conditions:diagnostic-file-position)
           0)
    (check "every diagnostic has a source form"
           (count-if #'null diags :key #'clef-conditions:diagnostic-source-form)
           0)
    (check-true "file positions are byte offsets (integers)"
                (every #'integerp
                       (mapcar #'clef-conditions:diagnostic-file-position diags)))

    ;; Classification, and crucially the symbol, without parsing English.
    (format t "~&classification~%")
    (let ((d (find-by-kind diags :undefined-function)))
      (check-true "undefined function found" d)
      (when d
        (check "  symbol" (symbol-name (clef-conditions:diagnostic-symbol d))
               "NO-SUCH-FUNCTION")
        (check "  severity" (clef-conditions:diagnostic-severity d) :style-warning)))

    (let ((d (find-by-kind diags :undefined-variable)))
      (check-true "undefined variable found" d)
      (when d
        (check "  symbol" (symbol-name (clef-conditions:diagnostic-symbol d))
               "NO-SUCH-VARIABLE")))

    (let ((d (find-by-kind diags :unused-variable)))
      (check-true "unused variable found" d)
      (when d
        (check "  symbol" (symbol-name (clef-conditions:diagnostic-symbol d))
               "NEVER-USED")
        (check "  severity" (clef-conditions:diagnostic-severity d) :style-warning)))

    (let ((d (find-by-kind diags :wrong-argument-count)))
      (check-true "wrong argument count found" d)
      (when d
        (check "  symbol" (symbol-name (clef-conditions:diagnostic-symbol d))
               "CALLS-UNDEFINED")
        (check "  severity" (clef-conditions:diagnostic-severity d) :warning)))

    ;; Positions differ per top-level form. Note the limit honestly: each error
    ;; above sits in a DIFFERENT defun, and FILE-POSITION names the enclosing
    ;; top-level form rather than the error, so two errors inside one defun
    ;; would legitimately share a position. Narrowing to the exact symbol is the
    ;; renderer's job (see render-tests), not this layer's.
    (format t "~&position per top-level form~%")
    (let ((positions (remove nil (mapcar #'clef-conditions:diagnostic-file-position diags))))
      (check "distinct forms get distinct positions"
             (length (remove-duplicates positions)) (length positions))))

  ;; Degradation: a condition signalled outside compilation still yields a
  ;; usable diagnostic, just without a location.
  (format t "~&degradation~%")
  (let ((d (handler-case (error "plain runtime error")
             (error (c) (clef-conditions:extract c)))))
    (check "runtime error has no file position"
           (clef-conditions:diagnostic-file-position d) nil)
    (check "runtime error severity" (clef-conditions:diagnostic-severity d) :error)
    (check "runtime error keeps its message"
           (clef-conditions:diagnostic-message d) "plain runtime error")
    (check "runtime error kind" (clef-conditions:diagnostic-kind d) :unknown))

  ;; Read errors arrive wrapped in SB-C:COMPILER-ERROR, which is NOT a subtype
  ;; of ERROR -- it is an encapsulated condition. Filtering on (or warning error)
  ;; drops it silently and a file with a bad package prefix reports nothing at
  ;; all. This pins both the unwrapping and the severity.
  (format t "~&encapsulated read errors~%")
  (let* ((diags (collect-diagnostics "(defun f () (no-such-pkg-xyz:g 1))"))
         (d (first diags)))
    (check-true "read error is reported at all" d)
    (when d
      (check "  severity is error, not note"
             (clef-conditions:diagnostic-severity d) :error)
      (check-true "  message names the package"
                  (search "NO-SUCH-PKG-XYZ"
                          (string-upcase (clef-conditions:diagnostic-message d))))))

  (run-render-tests)

  (format t "~&~%~A checks, ~A failure(s)~%" *checks* (length *failures*))
  (null *failures*))
