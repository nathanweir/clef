;;;; Corpus 3: CLOS as it is actually written.
;;;;
;;;; The old suite had DEFCLASS in five fixtures and DEFGENERIC/DEFMETHOD in
;;;; one apiece, none of them with qualifiers, specializers, EQL specialisers or
;;;; inheritance.

(defpackage :corpus-clos
  (:use :cl)
  (:export #:shape #:circle #:rectangle
           #:area #:describe-shape #:shape-name
           #:shape-error #:degenerate-shape))

(in-package :corpus-clos)

(defclass shape ()
  ((name :initarg :name
         :initform "unnamed"
         :accessor shape-name
         :documentation "Human-readable label.")
   (origin :initarg :origin
           :reader shape-origin
           :writer set-shape-origin))
  (:documentation "Base class for everything with an area."))

(defclass circle (shape)
  ((radius :initarg :radius :accessor circle-radius :type real))
  (:default-initargs :name "circle"))

(defclass rectangle (shape)
  ((width :initarg :width :accessor rectangle-width)
   (height :initarg :height :accessor rectangle-height)))

(define-condition shape-error (error)
  ((shape :initarg :shape :reader shape-error-shape))
  (:report (lambda (condition stream)
             (format stream "bad shape: ~A"
                     (shape-name (shape-error-shape condition))))))

(define-condition degenerate-shape (shape-error) ())

(defgeneric area (shape)
  (:documentation "Surface area of SHAPE."))

(defmethod area ((shape circle))
  (* pi (expt (circle-radius shape) 2)))

(defmethod area ((shape rectangle))
  (* (rectangle-width shape) (rectangle-height shape)))

;;; Method qualifiers -- :around, :before, :after -- which the grammar bundles
;;; into the defun-header alongside the name and lambda list.

(defmethod area :around ((shape shape))
  (let ((computed (call-next-method)))
    (if (zerop computed)
        (error 'degenerate-shape :shape shape)
        computed)))

(defmethod area :before ((shape circle))
  (check-type (circle-radius shape) real))

(defgeneric describe-shape (shape &key verbose)
  (:documentation "Describe SHAPE, optionally at length."))

(defmethod describe-shape ((shape shape) &key verbose)
  (if verbose
      (format nil "~A with area ~,2F" (shape-name shape) (area shape))
      (shape-name shape)))

;;; An EQL specialiser, and a method on a built-in class.

(defmethod describe-shape ((shape (eql :nothing)) &key verbose)
  (declare (ignore verbose))
  "nothing at all")

(defmethod print-object ((shape circle) stream)
  (print-unreadable-object (shape stream :type t)
    (format stream "r=~A" (circle-radius shape))))
