;;;; Corpus 1: packages, exports, and qualified references.
;;;;
;;;; Two of seventy-seven fixtures in the old suite contained a
;;;; package-qualified symbol, and both bugs found by pointing the server at
;;;; real code were package bugs. This file exists to make that path
;;;; unmissable.

(defpackage :corpus-geometry
  (:use :cl)
  (:nicknames :geo)
  (:export #:point
           #:make-point
           #:point-x
           #:point-y
           #:distance
           #:origin
           #:*unit*))

(defpackage :corpus-client
  (:use :cl)
  (:import-from :corpus-geometry #:distance)
  (:export #:describe-distance))

(in-package :corpus-geometry)

(defstruct point
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float))

(defparameter *unit* (make-point :x 1.0d0 :y 1.0d0)
  "A point one unit along each axis.")

(defun origin ()
  "The point at (0, 0)."
  (make-point))

(defun distance (a b)
  "Euclidean distance between two points."
  (let ((dx (- (point-x a) (point-x b)))
        (dy (- (point-y a) (point-y b))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;; A second package in the same file, referring to the first by qualified
;;; name -- both the single-colon external form and the double-colon internal
;;; form, which the reference index has never seen.

(in-package :corpus-client)

(defun describe-distance (a b)
  "Distance between A and B, as a string."
  (format nil "~,2F units apart" (corpus-geometry:distance a b)))

(defun uses-internal-symbol ()
  "Reaches an internal symbol with a double colon, which is legal and common."
  (corpus-geometry::make-point :x 3.0d0 :y 4.0d0))

(defun uses-nickname ()
  "Package nicknames are another spelling of the same reference."
  (geo:origin))

(defun round-trip ()
  (let ((p (uses-internal-symbol)))
    (describe-distance p (corpus-geometry:origin))))
