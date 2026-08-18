(in-package :clef-lsp/document)

;; DocumentHighlightKind constants (LSP spec)
(defconstant +highlight-kind-text+ 1 "A textual occurrence.")
(defconstant +highlight-kind-read+ 2 "Read-access of a symbol, like reading a variable.")
(defconstant +highlight-kind-write+ 3 "Write-access of a symbol, like writing to a variable.")

(defun handle-text-document-highlight (message)
  "Handle a textDocument/documentHighlight request.
Returns all occurrences of the symbol under cursor in the current document."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (file-path (clef-util:cleanup-path document-uri)))
    (slog :debug "[textDocument/documentHighlight] Document: ~A" document-uri)
    (slog :debug "[textDocument/documentHighlight] Position: line ~A, char ~A" line character)

    ;; Get the symbol at position (either a reference or definition)
    (multiple-value-bind (ref-name ref-scope)
        (get-ref-for-doc-pos document-uri line character)
      (declare (ignore ref-scope))

      ;; If no reference at position, check if we're on a definition
      (let ((symbol-name ref-name))
        (unless symbol-name
          (let ((def (find-definition-at-position document-uri line character)))
            (when def
              (setf symbol-name (clef-symbols:symbol-definition-symbol-name def))
              (slog :debug "[textDocument/documentHighlight] Found definition at point: ~A" symbol-name))))

        (unless symbol-name
          (slog :debug "[textDocument/documentHighlight] No symbol at position")
          (return-from handle-text-document-highlight #()))

        (slog :debug "[textDocument/documentHighlight] Symbol: ~A" symbol-name)

        ;; Find all occurrences in this file
        (let ((highlights '()))
          ;; Add all references in this file
          (let ((refs-tree (gethash file-path ctx:symbol-refs)))
            (when refs-tree
              (let ((all-refs (get-all-intervals-from-tree refs-tree)))
                (dolist (interval all-refs)
                  (let ((ref (clef-symbols::clef-interval-data interval)))
                    (when (and ref
                               (string= (clef-symbols:symbol-reference-symbol-name ref)
                                        symbol-name))
                      (push (make-highlight (clef-symbols:symbol-reference-node ref)
                                            +highlight-kind-read+)
                            highlights)))))))

          ;; Add definitions in this file
          (let ((scopes-tree (gethash file-path ctx:lexical-scopes)))
            (when scopes-tree
              (let ((all-scopes (get-all-intervals-from-tree scopes-tree)))
                (dolist (scope-interval all-scopes)
                  (let ((scope (clef-symbols::clef-interval-data scope-interval)))
                    (when scope
                      (dolist (def (clef-symbols:lexical-scope-symbol-definitions scope))
                        (when (string= (clef-symbols:symbol-definition-symbol-name def)
                                       symbol-name)
                          (let ((node (clef-symbols:symbol-definition-node def)))
                            (when node
                              (push (make-highlight node +highlight-kind-write+)
                                    highlights)))))))))))

          (slog :debug "[textDocument/documentHighlight] Found ~A highlights" (length highlights))
          (coerce (nreverse highlights) 'vector))))))

(defun make-highlight (node kind)
  "Create an LSP DocumentHighlight dict from a tree-sitter node."
  (dict "range" (node-to-lsp-range node)
        "kind" kind))
