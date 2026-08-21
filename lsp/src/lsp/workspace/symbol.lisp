(in-package :clef-lsp/workspace)

;; The SymbolKind constants and LISP-KIND-TO-LSP-KIND used to be defined here.
;; They now live in :clef-lsp/types/basic alongside NODE-TO-RANGE, since
;; textDocument/documentSymbol needs the same mapping and a second copy of a
;; lookup table is how two copies drift.

(defun handle-workspace-symbol (message)
  "Handle a workspace/symbol request.
Returns symbols matching the query from across the workspace."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (query (href params "query"))
         (query-upcase (string-upcase query)))
    (slog :debug "[workspace/symbol] Query: ~A" query)

    ;; Search the workspace symbol index for matching symbols
    (let ((results '()))
      (maphash (lambda (symbol-name defs)
                 ;; Match if query is a substring of symbol name (case-insensitive)
                 (when (or (string= query "")
                           (search query-upcase (string-upcase symbol-name)))
                   (dolist (def defs)
                     (push (symbol-def-to-symbol-info def symbol-name) results))))
               ctx:workspace-symbol-index)

      (slog :debug "[workspace/symbol] Found ~A matching symbols" (length results))

      ;; Return as vector, limit to reasonable number
      (let ((limited-results (if (> (length results) 100)
                                 (subseq results 0 100)
                                 results)))
        (coerce (nreverse limited-results) 'vector)))))

(defun symbol-def-to-symbol-info (def symbol-name)
  "Convert a symbol-definition to an LSP SymbolInformation dict."
  (let* ((location (clef-symbols:symbol-definition-location def))
         (file-path (clef-symbols:location-file-path location))
         (node (clef-symbols:symbol-definition-node def))
         (kind (clef-symbols:symbol-definition-kind def)))
    (dict "name" symbol-name
          "kind" (lisp-kind-to-lsp-kind kind)
          "location" (dict "uri" (format nil "file://~A" file-path)
                           "range" (node-to-range node)))))

;; NODE-TO-RANGE and LISP-KIND-TO-LSP-KIND are imported from
;; :clef-lsp/types/basic, which is where the one copy of each lives.
