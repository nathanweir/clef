;;;; What does textDocument/formatting actually promise?
;;;;
;;;; Two tests cover this endpoint: one asserts a response comes back, one
;;;; asserts the edit has a range. Neither asserts that the formatted text is
;;;; still the same program -- which is the only thing a formatter really owes
;;;; you, and the one whose failure destroys the user's file.
;;;;
;;;; Three questions, from the LSP spec and from what a formatter must not do:
;;;;
;;;;   1. Is the replace range correct? The handler builds it from
;;;;      GET-LAST-LINE-INFO, which returns a 1-INDEXED line number, while LSP
;;;;      positions are 0-indexed.
;;;;   2. Does formatting preserve the program? Read both texts as data and
;;;;      compare. Whitespace may move; forms may not.
;;;;   3. What happens on input the formatter cannot handle? The handler calls
;;;;      INDENTIFY with no handler around it, and replaces the WHOLE buffer
;;;;      with the result.

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

(defparameter *cases*
  '(("ordinary, badly indented"
     "(defun main ()
(let ((x 1))
(print x)))
")
    ("already well formed"
     "(defun main ()
  (let ((x 1))
    (print x)))
")
    ("no trailing newline"
     "(defun main () 42)")
    ("unbalanced -- what a buffer looks like mid-edit"
     "(defun main ()
  (let ((x 1))
    (pri")
    ("a string containing parens and a semicolon"
     "(defun main ()
  (format t \"a ) ; b~%\"))
")
    ("a block comment"
     "(defun main ()
  #| not ) code |#
  42)
")))

(defun forms-of (text)
  "Every top-level form in TEXT, or :UNREADABLE.

The comparison that matters: formatting may move whitespace and may not change
the program. Reading both sides as data is the only way to check that without
reimplementing the formatter."
  (handler-case
      (let ((*package* (find-package :cl-user))
            (*read-eval* nil)
            (forms '()))
        (with-input-from-string (in text)
          (loop
            (let ((form (read in nil :eof)))
              (when (eq form :eof) (return))
              (push form forms))))
        (nreverse forms))
    (error () :unreadable)))

(defmacro format-doc (uri)
  `(response-result-safe
    (call-handler "textDocument/formatting"
                  (dict "textDocument" (dict "uri" ,uri)
                        "options" (dict "tabSize" 2 "insertSpaces" t)))))

(with-direct-handler-test
  (init-server)
  (dolist (case *cases*)
    (destructuring-bind (name text) case
      (let* ((temp (write-temp-file text))
             (uri (format nil "file://~A" temp)))
        (call-handler "textDocument/didOpen"
                      (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                 "version" 1 "text" text))
                      :id nil)
        (format t "~&~%---------- ~A ----------~%" name)
        (let* ((lines (length (uiop:split-string text :separator '(#\Newline))))
               (result (handler-case (format-doc uri)
                         (error (e) (format t "  SIGNALLED: ~A~%" e) :signalled))))
          (cond
            ((eq result :signalled))
            ((or (not (or (vectorp result) (consp result)))
                 (zerop (length result)))
             ;; No edits is a legitimate and preferable answer when the text is
             ;; already formatted: replacing a file with itself dirties the
             ;; buffer and pushes an undo entry for nothing.
             (format t "  no edits~%"))
            (t
             ;; A list, not a vector -- every other handler in the server
             ;; returns a vector for a JSON array. Noted, not judged here.
             (let* ((edit (if (consp result) (first result) (aref result 0)))
                    (range (gethash "range" edit))
                    (end (gethash "end" range))
                    (end-line (gethash "line" end))
                    (new-text (gethash "newText" edit)))
               ;; The document has LINES lines, so its last valid line index is
               ;; LINES-1. An end line past that is a range the client has to
               ;; clamp for us.
               (format t "  document has ~D line(s); last valid index ~D~%"
                       lines (1- lines))
               (format t "  edit end line: ~D~A~%" end-line
                       (if (> end-line (1- lines)) "   <-- PAST END OF DOCUMENT" ""))
               (let ((before (forms-of text))
                     (after (forms-of new-text)))
                 (format t "  program preserved: ~A~%"
                         (cond
                           ((eq before :unreadable) "(input unreadable -- not applicable)")
                           ((eq after :unreadable) "NO -- output does not read")
                           ((equal before after) "yes")
                           (t "NO -- forms differ"))))))))
        (delete-temp-file temp)))))
