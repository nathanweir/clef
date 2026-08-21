;;;; Corpus 2: every binding form that actually shows up in Common Lisp.
;;;;
;;;; The old suite covered LET and function parameters. FLET appeared in one
;;;; fixture, LABELS in one, LOOP in one, and LET*, LAMBDA, HANDLER-CASE,
;;;; MULTIPLE-VALUE-BIND, DESTRUCTURING-BIND, DOLIST and DOTIMES in none at all.
;;;;
;;;; Shadowing is deliberate and repeated: the same name is bound at several
;;;; depths, so anything resolving by name rather than by binding gets it wrong.

(defpackage :corpus-bindings
  (:use :cl)
  (:export #:summarise #:tally #:transform-all))

(in-package :corpus-bindings)

(defun summarise (items)
  "LET and LET*, with the second binding depending on the first."
  (let* ((count (length items))
         (label (if (= count 1) "item" "items")))
    (let ((count (max count 0)))          ; shadows the outer COUNT
      (format nil "~D ~A" count label))))

(defun tally (numbers)
  "FLET and LABELS, both shadowing a parameter name."
  (flet ((numbers (n) (* n 2)))           ; shadows the parameter NUMBERS
    (labels ((walk (remaining acc)
               (if (null remaining)
                   acc
                   (walk (rest remaining)
                         (+ acc (numbers (first remaining)))))))
      (walk numbers 0))))

(defun transform-all (pairs)
  "DESTRUCTURING-BIND and MULTIPLE-VALUE-BIND."
  (mapcar (lambda (pair)
            (destructuring-bind (key value) pair
              (multiple-value-bind (quotient remainder) (floor value 10)
                (list key quotient remainder))))
          pairs))

(defun iterate-several-ways (limit)
  "DOLIST, DOTIMES and LOOP, each binding a variable the others also use."
  (let ((results '()))
    (dotimes (i limit)
      (push i results))
    (dolist (i results)
      (setf results (cons (* i 2) (rest results))))
    (loop :for i :from 0 :below limit
          :for doubled := (* i 2)
          :when (evenp doubled)
            :collect doubled :into evens
          :finally (return (append evens results)))))

(defun guarded (thunk)
  "HANDLER-CASE binds a condition variable, which is a binding like any other."
  (handler-case (funcall thunk)
    (division-by-zero (condition)
      (format nil "divide by zero: ~A" condition))
    (error (condition)
      (format nil "failed: ~A" condition))))

(defun uses-lambda ()
  "A LAMBDA parameter shadowing a global."
  (let ((scale 10))
    (mapcar (lambda (scale) (* scale scale))
            (list 1 2 scale))))
