(in-package :clef-lsp/document)

(defun handle-text-document-rename (message)
  "Handle a textDocument/rename request.
Returns a WorkspaceEdit with changes to rename the symbol at the given position."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (new-name (href params "new-name")))
    (slog :debug "[textDocument/rename] Document: ~A" document-uri)
    (slog :debug "[textDocument/rename] Position: line ~A, char ~A" line character)
    (slog :debug "[textDocument/rename] New name: ~A" new-name)

    (multiple-value-bind (ref-name ref-scope)
        (get-ref-for-doc-pos document-uri line character)
      (declare (ignore ref-scope))

      ;; If no symbol reference at position, check if we're on a symbol definition
      (let ((symbol-name ref-name)
            (definition-at-point nil))
        (unless symbol-name
          (let ((def (find-definition-at-position document-uri line character)))
            (when def
              (setf symbol-name (clef-symbols:symbol-definition-symbol-name def))
              (setf definition-at-point def)
              (slog :debug "[textDocument/rename] Found definition at point: ~A" symbol-name))))

        (unless symbol-name
          (slog :warn "[textDocument/rename] No symbol at position")
          (return-from handle-text-document-rename nil))

        (slog :debug "[textDocument/rename] Symbol name: ~A" symbol-name)

        ;; Find all references across all files
        (let ((all-locations (find-all-symbol-references symbol-name)))

          ;; Include the definition (search workspace index for cross-file definitions)
          (let ((definition (or definition-at-point
                                (first (clef-symbols:lookup-in-workspace-index symbol-name)))))
            (when (and definition (clef-symbols:symbol-definition-location definition))
              (push (symbol-definition-to-location definition) all-locations)))

          (slog :debug "[textDocument/rename] Found ~A location(s) to rename" (length all-locations))

          ;; Group locations by file URI for WorkspaceEdit
          (let ((changes-by-uri (make-hash-table :test 'equal)))
            ;; Group all edits by their file URI
            (dolist (location all-locations)
              (let* ((uri (href location "uri"))
                     (range (href location "range"))
                     (edit (dict "range" range
                                 "newText" new-name)))
                (push edit (gethash uri changes-by-uri))))

            ;; Convert hash table to the "changes" dict format
            (let ((changes (dict)))
              (maphash (lambda (uri edits)
                         (setf (gethash uri changes) (coerce (nreverse edits) 'vector)))
                       changes-by-uri)
              (dict "changes" changes))))))))

(defun handle-text-document-prepare-rename (message)
  "Handle a textDocument/prepareRename request.
Returns the range of the symbol that would be renamed, or null if rename is not valid."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character")))
    (slog :debug "[textDocument/prepareRename] Document: ~A" document-uri)
    (slog :debug "[textDocument/prepareRename] Position: line ~A, char ~A" line character)

    (multiple-value-bind (ref-name ref-scope)
        (get-ref-for-doc-pos document-uri line character)
      (declare (ignore ref-scope))

      ;; Check if we're on a symbol reference
      (let ((symbol-name ref-name)
            (symbol-node nil))

        ;; Try to get the node from the symbol reference
        (when ref-name
          (let* ((file-path (clef-util:cleanup-path document-uri))
                 (offset (clef-symbols::line-char-to-byte-offset file-path line character))
                 (refs-tree (gethash file-path clef-symbols:*symbol-refs-by-file*)))
            (when refs-tree
              (let ((intervals (interval:find-all refs-tree offset)))
                (when intervals
                  (let ((ref (clef-symbols::clef-interval-data (first intervals))))
                    (when ref
                      (setf symbol-node (clef-symbols:symbol-reference-node ref)))))))))

        ;; If no reference, check for definition
        (unless symbol-name
          (let ((def (find-definition-at-position document-uri line character)))
            (when def
              (setf symbol-name (clef-symbols:symbol-definition-symbol-name def))
              (setf symbol-node (clef-symbols:symbol-definition-node def))
              (slog :debug "[textDocument/prepareRename] Found definition at point: ~A" symbol-name))))

        (unless symbol-name
          (slog :debug "[textDocument/prepareRename] No symbol at position - rename not valid")
          (return-from handle-text-document-prepare-rename nil))

        (slog :debug "[textDocument/prepareRename] Symbol name: ~A" symbol-name)

        ;; Return the range and placeholder text
        (if symbol-node
            (dict "range" (node-to-range symbol-node)
                  "placeholder" symbol-name)
            ;; Fallback: just return the placeholder
            (dict "placeholder" symbol-name))))))
