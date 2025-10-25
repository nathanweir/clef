(in-package :clef-lsp/lifecycle)

(defhandler "initialize"
            (lambda (args)
                (format nil "in initialize ~A" (serapeum:pretty-print-hash-table args))))
