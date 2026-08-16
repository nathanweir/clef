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

(defun shallow-hash-vals (hash-table)
       (loop for key being the hash-keys of hash-table
             using (hash-value value)
             collect (list key value)))

(defun cleanup-path (root-uri)
       "Convert a 'file://' URI to a local pathname, optionally keeping any trailing slash."
       (let* ((without-file (cl-ppcre:regex-replace "^file://" root-uri ""))
              (no-trailing-slash (cl-ppcre:regex-replace "/$" without-file "")))
             (namestring (uiop:parse-native-namestring no-trailing-slash))))

(defun path-to-file-uri (path)
       "Convert PATH to a file:// URI, or NIL if it cannot be expressed as one.

        Definitions that resolve into SBCL's own sources come back from
        sb-introspect as logical pathnames (SYS:SRC;CODE;TARGET-HASH-TABLE.LISP).
        Formatting one straight into a URI yields file://SYS:SRC;CODE;... which
        is not a valid file URI and which no editor can open, so translate to a
        physical path first and refuse anything that is still not absolute."
       (when path
             (let* ((pathname (if (pathnamep path) path (pathname path)))
                    (physical (handler-case
                                (if (typep pathname 'logical-pathname)
                                    (translate-logical-pathname pathname)
                                    pathname)
                                ;; SYS: is only translatable when the SBCL source
                                ;; tree is actually present; it usually is not.
                                (error () nil))))
                  (when physical
                        (let ((native (handler-case (uiop:native-namestring physical)
                                        (error () nil))))
                             (when (and native
                                        (plusp (length native))
                                        (char= (char native 0) #\/)
                                        (probe-file physical))
                                   (format nil "file://~A" native)))))))

(defun read-file-text (file-path)
       "Read the entire contents of the file at FILE-PATH and return it as a string."
       (with-open-file (in file-path
                           :direction :input)
                       (let ((contents (let ((str (make-string (file-length in))))
                                            (read-sequence str in)
                                            str)))
                            contents)))
