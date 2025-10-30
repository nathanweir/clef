(in-package :clef-util)

(defun hash-table-to-instance (hash-table class)
    "Create an instance of CLASS using HASH-TABLE's keys/values as initargs.
CLASS should be a symbol naming a class.
HASH-TABLE should have keyword keys matching the class's initargs."
    (let ((initargs '()))
        (maphash (lambda (k v)
                     (push v initargs)
                     (push k initargs))
                 hash-table)
        (apply #'make-instance class (nreverse initargs))))

;; AI-slop; I haven't even read this
(defun hash-table-to-alist (hash)
    "Convert HASH (a hash table) to an alist of (key . value) pairs."
    (let (alist)
        (maphash (lambda (k v)
                     (push (cons k v) alist))
                 hash)
        (nreverse alist)))
