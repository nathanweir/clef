(in-package :clef-lsp/lifecycle)

(defun handle-initialize (request)
    (format nil "in handle-initialize ~A" (serapeum:pretty-print-hash-table request)))
