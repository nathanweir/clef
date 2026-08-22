;;;; Run the language server over a real codebase, not fabricated fixtures.
;;;;
;;;; Hand-written fixtures test constructs one at a time, which is not the same
;;;; thing as constructs used in tandem in a large program. Real source supplies
;;;; the combinations nobody would think to write, plus scale: cross-file
;;;; resolution, index size, and how long any of it takes.
;;;;
;;;; Defaults to clef's own source -- 122 files, ~17k lines, in-workspace, and
;;;; the code we actually edit daily, so any gap found here is a gap felt daily.
;;;; Pass a directory to point it elsewhere.
;;;;
;;;; Measures three things fixtures cannot:
;;;;   1. coverage at scale -- what fraction of real definitions get indexed
;;;;   2. cost -- indexing time per file and total
;;;;   3. crashes on code no one designed as a test case

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

(defparameter *target-dir*
  (or (second sb-ext:*posix-argv*) cl-user::*repo-root*))

(defparameter *skip-dirs* '("tmp" "build" ".direnv" ".git" "corpus"))

(defun interesting-file-p (path)
  "Is PATH a source file of the project under test?

Must check that the file is UNDER the target directory, not merely that its
name lacks a skipped component. SBCL's DIRECTORY resolves symlinks and returns
truenames, so .direnv's link into /nix/store comes back as /nix/store/... --
which contains no \"/.direnv/\" and sailed straight through a name-only filter.
That silently pulled ~105 files of SBCL and Quicklisp source into what was
reported as a sweep of clef's own code."
  (let* ((name (namestring path))
         (root (namestring (truename *target-dir*))))
    (and (eql 0 (search root name))
         (notany (lambda (d) (search (format nil "/~A/" d) name)) *skip-dirs*))))

(defun definition-name (form)
  "The name a top-level defining FORM introduces, or NIL. Mirrors 05's."
  (when (and (consp form) (symbolp (car form)))
    (let ((head (symbol-name (car form)))
          (name (second form)))
      (when (and (>= (length head) 3) (string-equal "DEF" (subseq head 0 3)))
        (let ((n (cond
                   ((and (consp name) (eq (car name) 'cl:setf))
                    (format nil "(setf ~(~A~))" (second name)))
                   ((symbolp name) (when name (string-downcase (symbol-name name))))
                   (t nil))))
          (when n (cons n (string-downcase head))))))))

(defun expected-names (path)
  "Definition names in PATH per the actual Lisp reader.

Real source names packages that may not exist in this image, and READ signals
on an unknown package marker. Unlike the corpus -- which we control -- that is
expected here, so an unreadable file yields NIL rather than aborting the run."
  (let ((names '())
        (*package* (find-package :cl-user))
        (*read-eval* nil))
    (handler-case
        (with-open-file (in path :external-format :utf-8)
          (loop
            (let ((form (handler-case (read in nil :eof)
                          (error () (return)))))
              (when (eq form :eof) (return))
              (when (and (consp form) (eq (car form) 'cl:in-package))
                (let ((pkg (ignore-errors (find-package (second form)))))
                  (when pkg (setf *package* pkg))))
              (let ((entry (definition-name form)))
                (when entry (pushnew entry names :test #'equal :key #'car))))))
      (error () nil))
    (nreverse names)))

(defmacro reported-names (uri)
  ;; A macro: CALL-HANDLER is an FLET inside WITH-DIRECT-HANDLER-TEST.
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

(let* ((files (remove-duplicates
               (remove-if-not #'interesting-file-p
                              (directory (merge-pathnames "**/*.lisp"
                                                          (truename *target-dir*))))
               ;; DIRECTORY can reach the same file by more than one path when
               ;; a directory below the root is a symlink. Without this the
               ;; tallies double-count and the file total disagreed with `find'
               ;; (205 vs 122), which is how the duplication showed up.
               :key (lambda (p) (namestring (truename p)))
               :test #'string=))
       (total-expected 0) (total-found 0)
       (worst '()) (errors '()) (slowest '())
       (grand-start (get-internal-real-time)))
  (defparameter cl-user::*missing-forms* '())
  (format t "~&Sweeping ~D real source file(s) under ~A~%~%"
          (length files) *target-dir*)
  (with-direct-handler-test
    (init-server)
    (dolist (path files)
      (let* ((text (handler-case (uiop:read-file-string path) (error () nil)))
             (expected (when text (expected-names path))))
        (when text
          (let* ((uri (format nil "file://~A" (namestring path)))
                 (start (get-internal-real-time))
                 (crashed nil))
            (handler-case
                (call-handler "textDocument/didOpen"
                              (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                         "version" 1 "text" text))
                              :id nil)
              (error (e) (setf crashed e) (push (cons path e) errors)))
            (let* ((ms (/ (- (get-internal-real-time) start) 1000.0))
                   (reported (unless crashed (reported-names uri)))
                   (missing (remove-if (lambda (e) (member (car e) reported :test #'string=))
                                       expected)))
              (push (cons (file-namestring path) ms) slowest)
              (when expected
                (incf total-expected (length expected))
                (incf total-found (- (length expected) (length missing)))
                (dolist (m missing) (push (cdr m) cl-user::*missing-forms*))
                (when missing
                  (push (list (file-namestring path)
                              (- (length expected) (length missing))
                              (length expected)
                              missing)
                        worst)))))))))

  (format t "~&========================================~%")
  (format t "~D of ~D definitions indexed (~D%)~%"
          total-found total-expected
          (if (plusp total-expected) (round (* 100 (/ total-found total-expected))) 100))
  (format t "total wall time: ~,1F s~%"
          (/ (- (get-internal-real-time) grand-start) 1000000.0))

  (let ((sorted (sort slowest #'> :key #'cdr)))
    (format t "~%slowest files to index:~%")
    (loop for (name . ms) in (subseq sorted 0 (min 8 (length sorted)))
          do (format t "  ~8,1F ms  ~A~%" ms name)))

  (when errors
    (format t "~%~D file(s) CRASHED the server:~%" (length errors))
    (dolist (e errors) (format t "  ~A: ~A~%" (file-namestring (car e)) (cdr e))))

  (format t "~%missing definitions by defining form:~%")
  (let ((tally (make-hash-table :test 'equal)))
    (dolist (f cl-user::*missing-forms*) (incf (gethash f tally 0)))
    (let ((pairs '()))
      (maphash (lambda (k v) (push (cons k v) pairs)) tally)
      (dolist (p (sort pairs #'> :key #'cdr))
        (format t "  ~5D  ~A~%" (cdr p) (car p)))))

  (format t "~%files swept, by top-level directory:~%")
  (let ((dirs (make-hash-table :test 'equal)))
    (dolist (f files)
      (let* ((rel (enough-namestring f (truename *target-dir*)))
             (top (subseq rel 0 (or (position #\/ rel) (length rel)))))
        (incf (gethash top dirs 0))))
    (let ((pairs '()))
      (maphash (lambda (k v) (push (cons k v) pairs)) dirs)
      (dolist (p (sort pairs #'> :key #'cdr))
        (format t "  ~5D  ~A~%" (cdr p) (car p)))))

  (let ((sorted (sort worst #'< :key (lambda (w) (/ (second w) (max 1 (third w)))))))
    (format t "~%worst-covered files:~%")
    (loop for (name found total missing) in (subseq sorted 0 (min 12 (length sorted)))
          do (format t "  ~3D/~3D  ~A~%" found total name)
             (format t "          missing: ~{~A~^ ~}~%"
                     (mapcar #'car (subseq missing 0 (min 8 (length missing))))))))
