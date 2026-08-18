;;;; Side-by-side demo: SBCL's default compiler output vs clef-conditions.
;;;;
;;;; Run: mise run conditions:demo
;;;;
;;;; Compiles one small file with four ordinary mistakes and prints what each
;;;; approach shows you.

(setf *compile-verbose* nil *compile-print* nil
      *load-verbose* nil *load-print* nil)

(defparameter *root*
  (make-pathname :directory (pathname-directory *load-truename*)))

(asdf:initialize-output-translations
 `(:output-translations
   ((,*root* :**/ :*.*.*) (,*root* "build" :**/ :*.*.*))
   :inherit-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "clef-conditions.asd" *root*))
  (asdf:load-system :clef-conditions))

(defparameter *report* *standard-output*)

(defparameter *source* "(defpackage :demo-pkg (:use :cl))
(in-package :demo-pkg)

(defun greet (name)
  (format nil \"hello, ~A\" nmae))

(defun total (items)
  (reduce #'+ items :initial-value 0))

(defun report ()
  (let ((unused-thing 42))
    (total 1 2 3)))
")

(defun demo ()
  (uiop:call-with-temporary-file
   (lambda (stream path)
     (write-string *source* stream)
     (force-output stream)
     (close stream)

     ;; --- 1. what SBCL shows you today ---
     (format *report* "~&~C[1m========== SBCL's default output ==========~C[0m~%~%"
             #\Escape #\Escape)
     (let ((sbcl-output
             (with-output-to-string (s)
               (let ((*error-output* s) (*standard-output* s))
                 (ignore-errors
                  (let ((f (compile-file path :verbose nil :print nil)))
                    (when (and f (probe-file f)) (delete-file f))))))))
       (write-string sbcl-output *report*)
       (format *report* "~&~C[2m(~D lines)~C[0m~%~%"
               #\Escape (count #\Newline sbcl-output) #\Escape))

     ;; --- 2. what clef-conditions shows you ---
     (format *report* "~&~C[1m========== clef-conditions ==========~C[0m~%~%"
             #\Escape #\Escape)
     (let ((clef-output
             (with-output-to-string (out)
               (let ((clef-conditions:*color* t)
                     (diags '()))
                 (handler-bind ((condition
                                  (lambda (c)
                                    (when (typep c '(or warning error))
                                      (push (clef-conditions:extract c) diags)))))
                   (let ((*error-output* (make-broadcast-stream))
                         (*standard-output* (make-broadcast-stream)))
                     (ignore-errors
                      (let ((f (compile-file path :verbose nil :print nil)))
                        (when (and f (probe-file f)) (delete-file f))))))
                 (dolist (d (nreverse diags))
                   (clef-conditions:render d :stream out)
                   (terpri out))))))
       (write-string clef-output *report*)
       (format *report* "~&~C[2m(~D lines)~C[0m~%"
               #\Escape (count #\Newline clef-output) #\Escape)))
   :want-stream-p t :want-pathname-p t :type "lisp" :keep nil))

(demo)
