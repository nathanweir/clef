;;;; What does textDocument/completion actually answer?
;;;;
;;;; The suite has three completion tests. Two are guarded so that they cannot
;;;; fail ("either success or error", `when result'), and the third asks for
;;;; completion with the cursor sitting on `helper' in `(helper)' -- a name that
;;;; is already complete. None of them describe the situation completion exists
;;;; for: a PARTIALLY TYPED name.
;;;;
;;;; The scenarios below come from the spec and from how Lisp is actually typed,
;;;; deliberately not from reading clef's handler. Per the LSP 3.17 spec,
;;;; completion is requested as the user types, so the document contains an
;;;; incomplete token and the position sits at its end. `isIncomplete' tells the
;;;; client whether to re-query on the next keystroke; a server returning a
;;;; filtered list should say false, a server returning everything should filter
;;;; or say true.
;;;;
;;;; Each case names what a working server owes the caller, then reports what
;;;; came back. No assertions -- this measures, it does not grade.

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
(setf clef-lsp/src/log:*log-mode* :none)
(handler-bind ((warning #'muffle-warning))
  (dolist (f '("test/framework.lisp"))
    (load (merge-pathnames f *lsp-root*))))

(in-package :clef-lsp/test/framework)

;;; Each scenario is (name expectation want text line character), where LINE and
;;; CHARACTER are 0-indexed and point just past the last character typed.
;;;
;;; WANT is the label a working server owes this request, or NIL where the
;;; correct answer is to offer nothing at all.
;;;
;;; The `|' in each expectation string marks the cursor for the reader's
;;; benefit; it is not in the document text.
(defparameter *scenarios*
  '(("partial name, own file"
     "typing `(hel|' should offer HELPER"
     "helper"
     "(defun helper () 42)
(defun main ()
  (hel"
     2 6)

    ("partial name, standard library"
     "typing `(form|' should offer FORMAT"
     "format"
     "(defun main ()
  (form"
     1 7)

    ("empty head position"
     "`(|' should offer something -- every callable in scope"
     "helper"
     "(defun helper () 42)
(defun main ()
  ("
     2 3)

    ("complete name already present"
     "the case the existing suite covers, kept for contrast"
     "helper"
     "(defun helper () 42)
(defun main ()
  (helper))"
     2 6)

    ("local LET binding"
     "typing `(al|' inside the body should offer ALPHA"
     "alpha"
     "(defun main ()
  (let ((alpha 1))
    (al"
     2 7)

    ("parameter"
     "typing `wid|' in the body should offer WIDGET"
     "widget"
     "(defun draw (widget)
  (wid"
     1 6)

    ("package-qualified"
     "typing `cl:for|' should offer CL:FORMAT"
     ;; The label must carry the qualification. A bare "format" would not start
     ;; with what the user typed, so the client's own filtering would drop it.
     "cl:format"
     "(defun main ()
  (cl:for"
     1 9)

    ("keyword"
     "typing `:doc|' should offer :DOCUMENTATION"
     ":documentation"
     "(defun main ()
  (list :doc"
     1 12)

    ("inside a string"
     "should offer NOTHING -- a string is not code"
     nil
     "(defun main ()
  \"hel"
     1 6)

    ("inside a comment"
     "should offer NOTHING -- a comment is not code"
     nil
     "(defun main ()
  ; hel"
     1 7)

    ("prefix does not match anything"
     "typing `(zzzz|' should offer nothing, or at least not everything"
     nil
     "(defun helper () 42)
(defun main ()
  (zzzz"
     2 7)))

(defmacro complete-at (uri line character)
  "The completion result for URI at LINE/CHARACTER, or NIL."
  `(response-result-safe
    (call-handler "textDocument/completion"
                  (dict "textDocument" (dict "uri" ,uri)
                        "position" (dict "line" ,line "character" ,character)))))

(defun labels-of (result)
  "The label strings in a completion result, deduplicated, sorted."
  (let ((items (and (hash-table-p result) (gethash "items" result))))
    (when (vectorp items)
      (sort (remove-duplicates
             (loop for i across items collect (gethash "label" i))
             :test #'equal)
            #'string<))))

(let ((total 0)
      (pass 0)
      (failures '()))
  (with-direct-handler-test
    (init-server)
    (dolist (scenario *scenarios*)
      (destructuring-bind (name expectation want text line character) scenario
        (incf total)
        (let* ((temp (write-temp-file text))
               (uri (format nil "file://~A" temp)))
          (call-handler "textDocument/didOpen"
                        (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                   "version" 1 "text" text))
                        :id nil)
          (let* ((result (complete-at uri line character))
                 (labels* (labels-of result))
                 (n (length labels*))
                 (has (and want (member want labels* :test #'string-equal) t))
                 (incomplete (and (hash-table-p result)
                                  (gethash "isIncomplete" result)
                                  t))
                 ;; Offering the right name among 979 is not the same as
                 ;; offering it: a list that was never narrowed is one the
                 ;; client must narrow itself.
                 ;;
                 ;; A long list is legitimate for an EMPTY prefix -- there is
                 ;; nothing to narrow by yet -- but only if the server says so
                 ;; with `isIncomplete', which is what tells the client to
                 ;; re-query as the prefix grows.
                 (ok (if want (and has (or (< n 100) incomplete)) (zerop n))))
            (if ok (incf pass) (push name failures))
            (format t "~&~%---------- ~A ----------~%" name)
            (format t "  expected: ~A~%" expectation)
            (format t "  isIncomplete: ~A~%"
                    (if (hash-table-p result)
                        (gethash "isIncomplete" result)
                        "(no result)"))
            (format t "  ~D item(s)~@[: ~{~A~^ ~}~]~%"
                    n
                    (when labels*
                      (if (> n 15)
                          (append (subseq labels* 0 15) (list "..."))
                          labels*)))
            (when want
              (format t "  wanted `~A': ~:[ABSENT~;present~]~%" want has))
            (format t "  => ~:[NO~;yes~]~%" ok))
          (delete-temp-file temp)))))

  (format t "~&~%========================================~%")
  (format t "~D of ~D scenarios answered usefully~%" pass total)
  (when failures
    (format t "~%Not useful:~%")
    (dolist (f (reverse failures)) (format t "  ~A~%" f))))
