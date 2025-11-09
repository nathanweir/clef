(in-package :clef-lsp/document)

(defun handle-text-document-definition (message)
       "Handle a textDocument/definition request."
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (href params "text-document" "uri"))
              (position (href params "position"))
              (line (href position "line"))
              (character (href position "character")))
             (slog :debug "[textDocument/definition] Document: ~A" document-uri)
             (slog :debug "[textDocument/definition] Position: line ~A, character ~A" line character)
             (clef-symbols:get-scope-for-doc-pos document-uri line character)
             ;; For now, we return nil as we have no definition support yet
             nil))
