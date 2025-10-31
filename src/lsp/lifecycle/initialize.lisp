(in-package :clef-lsp/lifecycle)


(defun handle-initialize (request)

       (let* ((params-hash (clef-jsonrpc/types:request-params request))
              (capabilities (gethash "capabilities" params-hash)))
             (setf clef-lsp/server:*client-capabilities* capabilities)

             ;; TODO: use *server-capabilities*
             ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult
             (dict "capabilities"
                   (dict "textDocumentSync" (dict "change" 2)
                         "documentFormattingProvider" t))))
