(require 'sb-introspect)

(+ 1 2)

(defun some-thing ()
       (let ((some-val 123))
            ;; asdfa
            (format t "sources 2: ~A~%" (sb-introspect:find-definition-sources-by-name 'some-val :variable))
            (format t "hello! ~A~%" some-val)))

(some-thing)

(format t "test 123~%")
(format t "sources 1: ~A~%" (sb-introspect:find-definition-sources-by-name 'some-thing :function))

(some-thing)
