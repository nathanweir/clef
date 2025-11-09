(in-package :clef-util)

(defun my-text-func (param-one param-two)
       (let ((my-let-var 123)
             (another-let-var 100))
            (format t "val is: ~A~%" (* my-let-var param-one another-let-var)))
       param-two)

;; This file only exists for life testing the symbol map creation. Delete long-term
(defparameter *my-cool-param* 123 "some text")
(defvar *my-cool-var* "an value")
