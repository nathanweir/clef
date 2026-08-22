;;;; Compile every corpus file, and report which ones are not valid Lisp.
;;;;
;;;; The corpus exists to exercise the language server against real Common
;;;; Lisp. A fixture that does not compile is not real Common Lisp and tests
;;;; nothing -- §3d of the LSP review found the older fixture set had drifted
;;;; exactly that way. This is the guard against drifting again.
;;;;
;;;; Each file is compiled in a fresh package-neutral context. Style warnings
;;;; are fine and expected (corpus code is deliberately odd). Full WARNINGs and
;;;; ERRORs are not.

(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)

(defparameter *corpus-dir*
  (merge-pathnames "corpus/" (directory-namestring *load-truename*)))

(defun check-file (path)
  "Compile PATH. Returns (values ok-p errors warnings)."
  (let ((errors 0) (warnings 0))
    (handler-bind ((style-warning #'muffle-warning)
                   (warning (lambda (c)
                              (incf warnings)
                              (format t "~&    warn: ~A~%" c)
                              (muffle-warning c))))
      (handler-case
          ;; COMPILE-FILE does NOT signal on a bad form -- it catches the error,
          ;; prints it, and reports through its THIRD return value. Relying on
          ;; HANDLER-CASE alone made this checker print "ok" directly underneath
          ;; a "caught ERROR", which is worse than no checker at all.
          (multiple-value-bind (fasl warnings-p failure-p)
              (compile-file path :verbose nil :print nil)
            (declare (ignore warnings-p))
            (when failure-p
              (incf errors)
              (format t "~&    compile-file reported failure~%"))
            (when (and fasl (not failure-p))
              (load fasl :verbose nil)))
        (error (e)
          (incf errors)
          (format t "~&    ERROR: ~A~%" e))))
    (values (and (zerop errors) (zerop warnings)) errors warnings)))

(let ((files (sort (directory (merge-pathnames "*.lisp" *corpus-dir*))
                   #'string< :key #'namestring))
      (bad 0))
  (format t "~&Compiling ~D corpus file(s)~%~%" (length files))
  (dolist (f files)
    (format t "  ~A~%" (file-namestring f))
    (multiple-value-bind (ok errors warnings) (check-file f)
      (declare (ignore errors))
      (declare (ignore warnings))
      (unless ok (incf bad))
      (format t "    ~A~%" (if ok "ok -- valid Common Lisp" "NOT VALID"))))
  (format t "~%========================================~%")
  (if (zerop bad)
      (format t "all ~D corpus file(s) compile~%" (length files))
      (format t "~D of ~D corpus file(s) do NOT compile~%" bad (length files)))
  (sb-ext:exit :code (if (zerop bad) 0 1)))
