;;;; End-to-end: what does a reader error look like now, rendered?
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/conditions/06-reader-render.lisp

(require :asdf)
(let ((root (make-pathname :directory (butlast (pathname-directory *load-truename*) 3))))
  (asdf:initialize-output-translations
   `(:output-translations ((,root :**/ :*.*.*) (,root "build" :**/ :*.*.*))
                          :inherit-configuration))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" root)))
(asdf:load-system :clef-conditions :verbose nil)

(defun show (label source)
  (let ((path (merge-pathnames (format nil "tmp/experiments/rr-~A.lisp" label)
                               (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string source s))
    (let ((diags '()))
      (handler-bind ((condition
                       (lambda (c)
                         (when (typep c '(or warning error sb-c:compiler-error))
                           (push (clef-conditions:extract c) diags)))))
        (let ((*error-output* (make-broadcast-stream))
              (*standard-output* (make-broadcast-stream)))
          (ignore-errors
           (let ((fasl (compile-file path :verbose nil :print nil)))
             (when (and fasl (probe-file fasl)) (delete-file fasl))))))
      (format t "~&~%========== ~A ==========~%" label)
      (dolist (d (nreverse diags))
        (format t "~&kind=~S severity=~S pos=~S~%"
                (clef-conditions:diagnostic-kind d)
                (clef-conditions:diagnostic-severity d)
                (clef-conditions:diagnostic-file-position d))
        (clef-conditions:render d)))))

(show "package-prefix" "(defun ok () 1)
(defun bad () (no-such-pkg-xyz:g 1))
")

(show "truncated" "(defun ok () 1)
(defun truncated () (+ 1 2)
")

(show "unbalanced-close" "(defun ok () 1)
(defun extra () 1))
(defun after () 2)
")
