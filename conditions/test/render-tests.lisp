(in-package :clef-conditions/test)

;;;; Tests for rendering. These assert on the located position, which is the
;;;; part that is easy to get wrong and was wrong at first: file-position names
;;;; the enclosing top-level form, not the error.

(defparameter *render-source* "(defpackage :rt-pkg (:use :cl))
(in-package :rt-pkg)

(defun greet (name)
  (format nil \"hi\" nmae))
")

(defun render-all (source)
  "Compile SOURCE and return (values list-of-rendered-strings octets)."
  (let ((octets (sb-ext:string-to-octets source :external-format :utf-8))
        (out '()))
    (uiop:call-with-temporary-file
     (lambda (stream path)
       (write-string source stream)
       (force-output stream)
       (close stream)
       (handler-bind ((condition
                        (lambda (c)
                          (when (typep c '(or warning error))
                            (push (clef-conditions:extract c) out)))))
         (let ((*error-output* (make-broadcast-stream))
               (*standard-output* (make-broadcast-stream)))
           (ignore-errors
            (let ((fasl (compile-file path :verbose nil :print nil)))
              (when (and fasl (probe-file fasl)) (delete-file fasl)))))))
     :want-stream-p t :want-pathname-p t :type "lisp" :keep nil)
    (values (mapcar (lambda (d)
                      (let ((clef-conditions:*color* nil))
                        (clef-conditions:render-to-string d :source octets)))
                    (nreverse out))
            octets)))

(defun run-render-tests ()
  (format t "~&rendering~%")
  (let ((rendered (render-all *render-source*)))
    (check-true "rendered something" (plusp (length rendered)))

    (let ((unused (find-if (lambda (s) (search "NAME is defined but never used" s))
                           rendered)))
      (check-true "unused-variable rendered" unused)
      (when unused
        ;; NAME is on line 4 of the source. Before the narrowing fix this
        ;; reported line 2 -- the position of the enclosing top-level form.
        (check-true "  points at line 4" (search ":4:" unused))
        (check-true "  underlines the symbol" (search "^^^^" unused))
        (check-true "  shows the source line" (search "(defun greet (name)" unused))
        (check-true "  names the enclosing definition" (search "in (DEFUN GREET)" unused))))

    (let ((undef (find-if (lambda (s) (search "undefined variable" s)) rendered)))
      (check-true "undefined-variable rendered" undef)
      (when undef
        ;; nmae is on line 5, and must NOT be confused with anything else.
        (check-true "  points at line 5" (search ":5:" undef))
        (check-true "  shows the offending line" (search "hi" undef))))

    ;; No ANSI escapes when colour is off -- the LSP protocol stream must stay
    ;; clean.
    (check "no escape codes when *color* is nil"
           (count-if (lambda (s) (find #\Escape s)) rendered)
           0))

  ;; A diagnostic with no location must still render rather than erroring.
  (let* ((d (handler-case (error "no location here")
              (error (c) (clef-conditions:extract c))))
         (text (clef-conditions:render-to-string d)))
    (check-true "locationless diagnostic still renders"
                (search "no location here" text))))
