;;;; Why are lexical bindings missing from completion?
;;;;
;;;; 09-completion-scenarios.lisp measured that ALPHA (a LET binding) and WIDGET
;;;; (a parameter) are absent from completion inside their own bodies, while
;;;; HELPER (a top-level definition in the same file) is present.
;;;;
;;;; Two candidate causes, and they need separating before anything is changed:
;;;;
;;;;   A. the scope chain walk starts at the wrong end, so inner scopes are
;;;;      never visited; or
;;;;   B. the inner scope does not EXIST, because the probe text was unbalanced
;;;;      -- which is what a buffer looks like at every keystroke, so this is
;;;;      the realistic case rather than an artifact of the probe.
;;;;
;;;; This prints the actual scope chain at a position, with each scope's kind
;;;; and the names it holds, for balanced and unbalanced spellings of the same
;;;; code. If the chain is identical and complete in the balanced case only,
;;;; the answer is B.

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

;;; (name want text line character)
(defparameter *cases*
  '(("LET body, UNBALANCED (what typing looks like)"
     "alpha"
     "(defun main ()
  (let ((alpha 1))
    (al"
     2 7)

    ("LET body, BALANCED, cursor on a complete name"
     "alpha"
     "(defun main ()
  (let ((alpha 1))
    (print alpha)))"
     2 12)

    ("parameter, UNBALANCED"
     "widget"
     "(defun draw (widget)
  (wid"
     1 6)

    ("parameter, BALANCED, cursor on a complete name"
     "widget"
     "(defun draw (widget)
  (print widget))"
     1 10)))

(defun scope-chain (file-path offset)
  "The scope chain at OFFSET, innermost first, as (kind . names) pairs."
  (let* ((scopes (ignore-errors
                  (interval:find-all (gethash file-path clef-context:lexical-scopes) offset)))
         (innermost (when scopes
                      (clef-symbols::clef-interval-data (first (last scopes)))))
         (chain '()))
    (loop for s = innermost then (clef-symbols:lexical-scope-parent-scope s)
          while s
          do (push (cons (clef-symbols:lexical-scope-kind s)
                         (mapcar #'clef-symbols:symbol-definition-symbol-name
                                 (clef-symbols:lexical-scope-symbol-definitions s)))
                   chain))
    (nreverse chain)))

(with-direct-handler-test
  (init-server)
  (dolist (case *cases*)
    (destructuring-bind (name want text line character) case
      (let* ((temp (write-temp-file text))
             (uri (format nil "file://~A" temp))
             (path (clef-util:cleanup-path uri)))
        (call-handler "textDocument/didOpen"
                      (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                 "version" 1 "text" text))
                      :id nil)
        (format t "~&~%---------- ~A ----------~%" name)
        (let ((offset (clef-symbols:line-char-to-byte-offset path line character)))
          (multiple-value-bind (ref-name ref-scope)
              (clef-symbols:get-ref-for-doc-pos uri line character)
            (format t "  offset ~A   ref-name: ~S   ref-scope kind: ~S~%"
                    offset ref-name
                    (when ref-scope (clef-symbols:lexical-scope-kind ref-scope))))
          (format t "  scope chain, innermost first:~%")
          (let ((chain (scope-chain path offset)))
            (if (null chain)
                (format t "    (none)~%")
                (dolist (link chain)
                  (let ((names (cdr link)))
                    (format t "    ~A~24T~D name(s)~@[: ~{~A~^ ~}~]~%"
                            (car link) (length names)
                            (if (> (length names) 8)
                                (append (subseq names 0 8) (list "..."))
                                names))))))
          (format t "  `~A' reachable in chain: ~:[NO~;yes~]~%"
                  want
                  (member want (loop for link in (scope-chain path offset)
                                     append (cdr link))
                          :test #'string-equal)))
        (delete-temp-file temp)))))
