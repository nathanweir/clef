(in-package :clef-lsp/lifecycle)

(defun handle-initialized (request)
       (declare (ignore request))
       (setf clef-lsp/server:*initialized* t)
       ;; Send no response
       nil)
