(in-package :clef-lsp/document)

(defun handle-text-document-did-open (message)
       (let* ((params-hash (clef-jsonrpc/types:request-params message))
              (document-uri (href params-hash "text-document" "uri"))
              (document-text (href params-hash "text-document" "text")))
             (setf (gethash document-uri ctx:documents) document-text)))
