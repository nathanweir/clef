(in-package :clef-lsp/document)

(defun handle-text-document-did-save (message)
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (clef-util:cleanup-path (href params "text-document" "uri"))))
             (slog :debug "[textDocument/didSave] Document saved: ~A" document-uri)
             ;; For .asd files, reload the system definition
             (when (uiop:string-suffix-p document-uri ".asd")
                   (clef-lsp/lifecycle:load-asd document-uri))
             ;; Rebuild symbol map for the saved file
             (let ((document-text (gethash (format nil "file://~A" document-uri)
                                           clef-lsp/server:*documents*)))
                  (when document-text
                        (clef-symbols:build-file-symbol-map document-uri document-text)))))
