(in-package :clef-lsp/workspace)

; (defun get-syntax-errors (input-text)
;        "Parse Lisp source code and emit a Diagnostic for each syntax error."
;        (let* ((tree (alive/parser:parse-string input-text))
;               (errors (collect-error-nodes tree))
;               (diagnostics '()))
;              (dolist (err errors)
;                      (destructuring-bind ((start-line start-col) (end-line end-col)) (cdr err)
;                                          (push (make-instance 'diagnostic:diagnostic
;                                                               :range (range:create
;                                                                        (pos:create start-line start-col)
;                                                                        (pos:create end-line end-col))
;                                                               :severity diagnostic:+error+
;                                                               :message "Syntax error detected.")
;                                                diagnostics)))
;              diagnostics))


(defun handle-workspace-diagnostic (message)
       (declare (ignore message))
       (dict "items" #()))
