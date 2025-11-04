(in-package :clef-lsp/document)

(defun handle-text-document-did-save (message)
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (clef-util:cleanup-path (href params "text-document" "uri"))))
             (slog :debug "[textDocument/didSave] Loading saved file: ~A" document-uri)
             (if (uiop:string-suffix-p document-uri ".asd" )
                 (clef-lsp/lifecycle:load-asd document-uri)
                 ;; TODO: Consider reloading the relevant system instead
                 ;; might be fast enougH? using load causes errors with defconstant
                 ;; Or, swallow those errors and just inform users that defconstant reloads won't work
                 (load document-uri))))
