(in-package :clef-lsp/document)

(defun handle-text-document-did-open (message)
    (let* ((params-hash (clef-jsonrpc/types:request-params message))
           (document-uri (href params-hash "text-document" "uri"))
           (document-text (href params-hash "text-document" "text")))
        (slog :info "Document opened: ~A~%" document-uri)
        ;; Do NOT split the doc by newlines; LSP requests send info on text ranges and we calculate
        ;; newlines by (for now) an assumption that a newline is always "\n"
        (setf (gethash document-uri clef-lsp/server:*documents*) document-text))
    nil)
