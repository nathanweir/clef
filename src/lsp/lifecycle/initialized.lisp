(in-package :clef-lsp/lifecycle)

(defun handle-initialized (request)
    (setf clef-lsp/server:*initialized* t)
    ;; Send no response
    nil)
