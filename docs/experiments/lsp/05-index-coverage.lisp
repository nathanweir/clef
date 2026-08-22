;;;; Does clef actually INDEX what the corpus defines?
;;;;
;;;; The existing sweep (03-corpus-sweep.lisp) checks robustness: nothing
;;;; crashes, every request is answered, every range is sane. It passed the
;;;; whole corpus while 12-reader.lisp reported five document symbols for a file
;;;; defining twenty-odd names -- because "five sane ranges" satisfies every
;;;; invariant it knows how to check.
;;;;
;;;; This measures the other thing: COVERAGE. For each corpus file, extract the
;;;; names defined at top level by reading the source as data, then ask
;;;; documentSymbol what clef found, and name the difference.
;;;;
;;;; Reading with READ rather than by regex matters -- it means the expected set
;;;; is computed by the actual Lisp reader, so reader conditionals, block
;;;; comments and dotted syntax are all handled correctly and for free.

#-quicklisp
(let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file init) (load init)))
(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)
(defparameter *repo-root* (truename "."))
(defparameter *lsp-root* (merge-pathnames "lsp/" *repo-root*))
(require 'sb-posix) (require 'sb-introspect)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *repo-root*))
  (asdf:load-asd (merge-pathnames "clef-lsp.asd" *lsp-root*))
  (asdf:load-system :clef-lsp))
(ql:quickload '(:serapeum :bordeaux-threads :com.inuoe.jzon :babel :cl-ppcre) :silent t)
(setf clef-log:*log-mode* :none)
(handler-bind ((warning #'muffle-warning))
  (dolist (f '("test/package.lisp" "test/framework.lisp"))
    (load (merge-pathnames f *lsp-root*))))

(in-package :clef-test)

(defparameter *corpus-dir*
  (merge-pathnames "docs/experiments/lsp/corpus/" cl-user::*repo-root*))

(defun definition-name (form)
  "The name a top-level defining FORM introduces, as a lowercase string, or NIL.

Handles the two shapes a name can take: a symbol, and a list -- (setf foo) is a
legal function name and appears in real code."
  (when (and (consp form) (symbolp (car form)))
    (let ((head (symbol-name (car form)))
          (name (second form)))
      (when (and (>= (length head) 3) (string-equal "DEF" (subseq head 0 3)))
        (cond
          ((and (consp name) (eq (car name) 'cl:setf))
           (format nil "(setf ~(~A~))" (second name)))
          ((symbolp name) (when name (string-downcase (symbol-name name))))
          (t nil))))))

(defun expected-names (path)
  "Every top-level definition name in PATH, read by the actual Lisp reader."
  (let ((names '())
        (*package* (find-package :cl-user))
        ;; T, not NIL. With *READ-EVAL* NIL the reader signals on `#.', and
        ;; READ cannot resynchronise afterwards -- so the expected list stopped
        ;; at the first read-time-eval and silently UNDERCOUNTED every
        ;; definition after it. 12-reader.lisp reported 16 expected when it
        ;; defines considerably more.
        ;;
        ;; Safe only because the corpus is ours and is compile-checked by
        ;; compile-corpus.lisp. This is a local measurement over trusted files,
        ;; not something the server does to user code -- clef itself must never
        ;; evaluate `#.'.
        (*read-eval* t))
    (with-open-file (in path :external-format :utf-8)
      (loop
        (let ((form (handler-case (read in nil :eof)
                      ;; A form we cannot read is not a coverage question.
                      (error () (return)))))
          (when (eq form :eof) (return))
          ;; IN-PACKAGE affects how later forms read; follow it where we can.
          (when (and (consp form) (eq (car form) 'cl:in-package))
            (let ((pkg (find-package (second form))))
              (when pkg (setf *package* pkg))))
          (let ((name (definition-name form)))
            (when name (pushnew name names :test #'string=))))))
    (nreverse names)))

;;; A MACRO, not a function. CALL-HANDLER is an FLET established by
;;; WITH-DIRECT-HANDLER-TEST, so anything defined outside that macro cannot see
;;; it -- a DEFUN here fails at runtime with "The function CALL-HANDLER is
;;; undefined". This is documented on WITH-DIRECT-HANDLER-TEST and has caught
;;; me more than once.
(defmacro reported-names (uri)
  "Names clef reports for URI via textDocument/documentSymbol."
  `(let ((result (response-result-safe
                  (call-handler "textDocument/documentSymbol"
                                (dict "textDocument" (dict "uri" ,uri))))))
     (when (vectorp result)
       (loop for s across result
             append (cons (string-downcase (gethash "name" s))
                          (let ((kids (gethash "children" s)))
                            (when (vectorp kids)
                              (loop for k across kids
                                    collect (string-downcase (gethash "name" k))))))))))

(let ((files (sort (directory (merge-pathnames "*.lisp" *corpus-dir*))
                   #'string< :key #'namestring))
      (total-expected 0)
      (total-found 0)
      (gaps '()))
  (with-direct-handler-test
    (init-server)
    (dolist (path files)
      (let* ((text (uiop:read-file-string path))
             (expected (expected-names path))
             (temp (write-temp-file text))
             (uri (format nil "file://~A" temp)))
        (call-handler "textDocument/didOpen"
                      (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                 "version" 1 "text" text))
                      :id nil)
        (let* ((reported (reported-names uri))
               (missing (remove-if (lambda (n) (member n reported :test #'string=))
                                   expected)))
          (incf total-expected (length expected))
          (incf total-found (- (length expected) (length missing)))
          (format t "~&~%========== ~A ==========~%" (file-namestring path))
          (format t "  ~D of ~D top-level definitions indexed~@[  (~D%)~]~%"
                  (- (length expected) (length missing)) (length expected)
                  (when (plusp (length expected))
                    (round (* 100 (/ (- (length expected) (length missing))
                                     (length expected))))))
          (when missing
            (push (cons (file-namestring path) missing) gaps)
            (format t "  MISSING:~%")
            (dolist (m missing) (format t "    ~A~%" m))))
        (delete-temp-file temp))))

  (format t "~&~%========================================~%")
  (format t "~D of ~D definitions indexed across the corpus (~D%)~%"
          total-found total-expected
          (if (plusp total-expected) (round (* 100 (/ total-found total-expected))) 100))
  (when gaps
    (format t "~%~D file(s) with gaps~%" (length gaps))))
