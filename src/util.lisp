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

;; TODO: For testing, remove
(defparameter *my-cool-param* 123)

(defmacro a-while-macro (condition &body body)
          `(loop while ,condition do (progn ,@body)))

;; AI-slop; I haven't even read this
(defun hash-table-to-alist (hash)
       "Convert HASH (a hash table) to an alist of (key . value) pairs."
       (let (alist)
            (maphash (lambda (k v)
                             (push (cons k v) alist))
                     hash)
            (nreverse alist)))

(defun shallow-hash-vals (hash-table)
       (loop for key being the hash-keys of hash-table
             using (hash-value value)
             collect (list key value)))

(defun cleanup-path (root-uri)
       "Convert a 'file://' URI to a local pathname, removing any trailing slash."
       (let* ((without-file (cl-ppcre:regex-replace "^file://" root-uri ""))
              (no-trailing-slash (cl-ppcre:regex-replace "/$" without-file "")))
             (namestring (uiop:parse-native-namestring no-trailing-slash))))

(defun read-file-text (file-path)
       "Read the entire contents of the file at FILE-PATH and return it as a string."
       (with-open-file (in file-path
                           :direction :input)
                       (let ((contents (let ((str (make-string (file-length in))))
                                            (read-sequence str in)
                                            str)))
                            contents)))
