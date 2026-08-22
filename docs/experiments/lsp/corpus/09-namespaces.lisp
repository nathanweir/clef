;;;; One name, many namespaces.
;;;;
;;;; The structural fact most likely to be got wrong by a server written with a
;;;; single-namespace mental model. See docs/surveys/cl-surface-area.md §1.
;;;;
;;;; In Common Lisp a symbol is not one binding. TREE below is simultaneously a
;;;; function, a special variable, a type, a class, a block name and a tagbody
;;;; tag. None of these shadow each other; all six coexist; each is reached by
;;;; a different syntactic position. Go-to-definition on TREE therefore has SIX
;;;; correct answers, and which one is right depends entirely on where the
;;;; cursor is.
;;;;
;;;; If clef resolves all of these to whichever definition it indexed first,
;;;; every one but that one is a navigation bug.

(defpackage :clef-corpus/namespaces (:use :cl))
(in-package :clef-corpus/namespaces)

;;; --- the same name, six times --------------------------------------------

;;; 1. the FUNCTION namespace
(defun tree (x) (list :function x))

;;; 2. the VARIABLE namespace -- a different binding entirely
(defvar *tree* :variable)

;;; 3. the TYPE namespace
(deftype tree () '(or cons null))

;;; 4. the CLASS namespace. Note this ALSO defines a type named TREE, and a
;;;    class and a deftype of the same name would conflict -- so the class gets
;;;    its own name and the point is made by BRANCH below.
(defclass branch ()
  ((tree :initarg :tree :accessor branch-tree)))   ; 5. a SLOT named TREE

;;; A structure whose accessor collides by name with the function above is
;;; legal too, so long as the packages differ. Here it does not collide because
;;; DEFSTRUCT prefixes.
(defstruct grove (tree nil))                        ; defines GROVE-TREE

(defun exercise-namespaces (input)
  "Every TREE below is a DIFFERENT binding. Six occurrences, six meanings."
  (block tree                            ; 6. the BLOCK namespace
    (let ((tree input))                  ; 7. a LEXICAL VARIABLE named TREE
      (tagbody
         (when (null tree) (go tree))    ; 8. the TAGBODY TAG namespace
         (return-from tree                ; refers to the BLOCK, not the variable
           (list (tree tree)              ; function call, then variable read
                 (the tree tree)          ; TYPE position, then variable
                 *tree*                   ; the special variable
                 #'tree))                 ; the FUNCTION object
       tree                               ; the tag itself
         (return-from tree :was-null)))))

;;; --- function and variable of the same name, side by side ----------------

(defvar list-length-cache nil)

(defun demonstrate-fn-vs-var (list-length-cache)
  "LIST-LENGTH-CACHE is a parameter here, shadowing the special above. The
call to LIST-LENGTH is the standard function -- same first word, different
namespace and different symbol."
  (list (list-length '(1 2 3))
        list-length-cache))

;;; --- shadowing a STANDARD name lexically ---------------------------------

(defun shadows-standard-names (list string)
  "LIST and STRING are parameters here, shadowing the standard TYPE names of
the same spelling. The type positions still refer to the types."
  (declare (type cl:list list) (type cl:string string))
  (let ((length (length list)))          ; LENGTH the variable vs LENGTH the function
    (list length string)))

;;; --- SETF functions are a namespace wrinkle of their own -----------------

(defun place (obj) (car obj))
(defun (setf place) (new obj) (setf (car obj) new))

(defun exercise-setf-namespace (obj)
  "PLACE is read through one definition and written through another. Both are
named PLACE; only one is a symbol."
  (setf (place obj) 1)
  (place obj))
