;;;; Does an externally-proclaimed ftype against a library function you do not
;;;; own actually produce call-site type checking in SBCL?
;;;;
;;;; This gates roadmap W4 tier 3 ("CL's DefinitelyTyped"): shipping type
;;;; declarations for a curated library set without touching upstream source.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load 01-external-ftype.lisp

(ql:quickload :cl-ppcre :silent t)

(defvar *captured* nil)

(defmacro probing ((label) &body body)
  "Compile BODY's forms, capturing every warning the compiler signals."
  `(let ((*captured* nil))
     (handler-bind ((warning (lambda (w)
                               (push (list (type-of w) (princ-to-string w)) *captured*)
                               (muffle-warning w))))
       ,@body)
     (format t "~&~%=== ~A ===~%" ,label)
     (if (null *captured*)
         (format t "  no warnings~%")
         (dolist (w (reverse *captured*))
           (format t "  [~A]~%    ~A~%" (first w) (second w))))
     (length *captured*)))

(format t "~&SBCL ~A~%" (lisp-implementation-version))

;;; ---------------------------------------------------------------------------
;;; 1. Baseline. cl-ppcre:quote-meta-chars takes a string. Call it with an
;;;    integer, with no proclamation of our own. Does SBCL notice unaided?
;;; ---------------------------------------------------------------------------

(probing ("1. bad call, NO external proclamation")
  (compile nil '(lambda () (cl-ppcre:quote-meta-chars 42))))

;;; ---------------------------------------------------------------------------
;;; 2. Now proclaim the type from outside the library -- we do not touch
;;;    cl-ppcre's source -- and make the same bad call. This is the whole
;;;    question: does SBCL trust and enforce our external declaration?
;;; ---------------------------------------------------------------------------

(declaim (ftype (function (string) string) cl-ppcre:quote-meta-chars))

(probing ("2. bad call, WITH external proclamation")
  (compile nil '(lambda () (cl-ppcre:quote-meta-chars 42))))

;;; ---------------------------------------------------------------------------
;;; 3. A correct call under the same proclamation should stay quiet, otherwise
;;;    the mechanism is useless from false positives.
;;; ---------------------------------------------------------------------------

(probing ("3. good call, WITH external proclamation")
  (compile nil '(lambda () (cl-ppcre:quote-meta-chars "hello"))))

;;; ---------------------------------------------------------------------------
;;; 4. Does the proclamation propagate into inference? If a caller's return
;;;    value is known to be a string, using it as one should be fine and using
;;;    it as a number should not.
;;; ---------------------------------------------------------------------------

(probing ("4. return type used as a number")
  (compile nil '(lambda (s)
                 (declare (type string s))
                 (1+ (cl-ppcre:quote-meta-chars s)))))

;;; ---------------------------------------------------------------------------
;;; 5. What happens when the proclamation CONTRADICTS the real definition?
;;;    Declare a different function as taking an integer when it takes a string,
;;;    then call it correctly per the real signature.
;;; ---------------------------------------------------------------------------

(declaim (ftype (function (integer) integer) cl-ppcre:scan-to-strings))

(probing ("5. contradictory proclamation, call per REAL signature")
  (compile nil '(lambda () (cl-ppcre:scan-to-strings "a" "abc"))))

;;; ---------------------------------------------------------------------------
;;; 6. Does the runtime actually enforce it, or is this compile-time only?
;;; ---------------------------------------------------------------------------

(format t "~&~%=== 6. runtime behaviour of the bad call ===~%")
(handler-case
    (let ((f (let ((*error-output* (make-broadcast-stream)))
               (compile nil '(lambda ()
                              (declare (optimize (safety 3)))
                              (cl-ppcre:quote-meta-chars 42))))))
      (format t "  compiled; calling it...~%")
      (finish-output)
      (format t "  returned: ~S~%" (funcall f)))
  (error (e) (format t "  signalled ~A: ~A~%" (type-of e) e)))

(format t "~&~%done~%")
