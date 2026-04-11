(in-package :clef-lsp/lifecycle)

(defun handle-initialized (request)
       (declare (ignore request))
       (setf ctx:initialized t)
       ;; Send no response
       nil)
