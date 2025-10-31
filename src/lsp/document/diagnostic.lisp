(in-package :clef-lsp/document)

;; TODO: I was originally putting all of these types into :clef-lsp/types, but I dislike that now
;; that I've (for now) given up on fully replicating the LSP spec types in CLOS. Consider where this
;; should live long-term.

(defconstant +diagnostic-severity-error+ 1 "Reports an error.")
(defconstant +diagnostic-severity-warning+ 2 "Reports a warning.")
(defconstant +diagnostic-severity-information+ 3 "Reports an information.")
(defconstant +diagnostic-severity-hint+ 4 "Reports a hint.")

;; Utils; need to be place elsewhere and long-term turned into classes/structs/Coalton types (something)

(defun make-position (line char)
       (dict "line" line "character" char))

(defun make-range (start-line start-char end-line end-char)
       (dict "start" (make-position start-line start-char)
             "end" (make-position end-line end-char)))

(defun get-syntax-errors (input-text)
       "Parse Lisp source code and emit a Diagnostic for each syntax error."
       (let* ((tree (clef-parser/parser:parse-string input-text))
              (errors (collect-error-nodes tree))
              (diagnostics '()))
             (dolist (err errors)
                     (destructuring-bind ((start-line start-col) (end-line end-col)) (cdr err)

                                         (push (dict "range" (make-range start-line start-col end-line end-col)
                                                     "severity" +diagnostic-severity-error+
                                                     "message" "Syntax error")
                                               diagnostics)))
             diagnostics))

(defun collect-error-nodes (node)
       "Return a list of (TYPE RANGE) for all error nodes in the tree."
       (let ((results '()))
            (labels ((walk (n)
                           (let ((type (ts:node-type n)))
                                (when (or (eql type :error) (eql type :missing))
                                      (push (cons type (ts:node-range n)) results))
                                (dolist (child (ts:node-children n))
                                        (walk child)))))
                    (walk node))
            results))


(defun handle-text-document-diagnostic (message)
       (let* ((document-uri (href (clef-jsonrpc/types:request-params message)
                                  "text-document"
                                  "uri"))
              (document-text (gethash document-uri clef-lsp/server:*documents*))
              (items (get-syntax-errors document-text)))
             (dict "kind" "full"
                   "items" items)))
