(in-package :clef-lsp/document)

(defun handle-text-document-references (message)
  "Handle a textDocument/references request.
Returns all locations where the symbol at the given position is referenced."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (context (href params "context"))
         (include-declaration (and context (href context "include-declaration"))))
    (slog :debug "[textDocument/references] Document: ~A" document-uri)
    (slog :debug "[textDocument/references] Position: line ~A, char ~A" line character)
    (slog :debug "[textDocument/references] Include declaration: ~A" include-declaration)

    (multiple-value-bind (ref-name ref-scope)
        (get-ref-for-doc-pos document-uri line character)

      ;; If no symbol reference at position, check if we're on a symbol definition
      ;; (e.g., the function name in a defun)
      (let ((symbol-name ref-name)
            (definition-at-point nil))
        (unless symbol-name
          (let ((def (find-definition-at-position document-uri line character)))
            (when def
              (setf symbol-name (clef-symbols:symbol-definition-symbol-name def))
              (setf definition-at-point def)
              (slog :debug "[textDocument/references] Found definition at point: ~A" symbol-name))))

        (unless symbol-name
          (slog :warn "[textDocument/references] No symbol at position")
          (return-from handle-text-document-references #()))

        (slog :debug "[textDocument/references] Symbol name: ~A" symbol-name)

        ;; Find all references across all files
        (let ((all-references (find-all-symbol-references symbol-name)))
          (slog :debug "[textDocument/references] Found ~A reference(s)" (length all-references))

          ;; Optionally include the definition
          (when include-declaration
            (let ((definition (or definition-at-point
                                  (search-up-for-symbol-def ref-scope symbol-name))))
              (when (and definition (clef-symbols:symbol-definition-location definition))
                (push (symbol-definition-to-location definition) all-references))))

          ;; Convert to LSP Location array
          (if all-references
              (coerce all-references 'vector)
              #()))))))

(defun find-definition-at-position (document-uri line character)
  "Find a symbol definition at the given position.
This handles the case where the cursor is on a function/variable name in a definition
(e.g., the 'foo' in '(defun foo ...)'), rather than on a usage."
  (let* ((file-path (clef-util:cleanup-path document-uri))
         (offset (clef-symbols:line-char-to-byte-offset file-path line character)))
    ;; Get the lexical scope at this position
    (let ((scopes (interval:find-all
                   (gethash file-path ctx:lexical-scopes)
                   offset)))
      ;; Check each scope (from innermost to outermost) for definitions at this position
      (dolist (scope-interval scopes)
        (let ((scope (clef-symbols::clef-interval-data scope-interval)))
          (when scope
            ;; Check symbol definitions in this scope
            (dolist (def (clef-symbols:lexical-scope-symbol-definitions scope))
              (let ((def-node (clef-symbols:symbol-definition-node def)))
                (when def-node
                  ;; Check if cursor is within this definition's node
                  (let* ((start-row (clef-parser/parser:node-start-point-row def-node))
                         (start-col (clef-parser/parser:node-start-point-column def-node))
                         (end-row (clef-parser/parser:node-end-point-row def-node))
                         (end-col (clef-parser/parser:node-end-point-column def-node)))
                    (when (position-in-range-p line character
                                               start-row start-col
                                               end-row end-col)
                      (return-from find-definition-at-position def))))))))))))

(defun position-in-range-p (line char start-line start-char end-line end-char)
  "Check if position (line, char) is within the range [start, end]."
  (cond
    ;; Before start
    ((< line start-line) nil)
    ((and (= line start-line) (< char start-char)) nil)
    ;; After end
    ((> line end-line) nil)
    ((and (= line end-line) (>= char end-char)) nil)
    ;; Within range
    (t t)))

(defun find-all-symbol-references (symbol-name)
  "Find all references to SYMBOL-NAME across all files in the workspace.
Returns a list of LSP Location dicts."
  (let ((locations '()))
    ;; Search through all files' symbol reference trees
    (maphash (lambda (file-path refs-tree)
               (when refs-tree
                 ;; Walk all intervals in the tree to find matching symbol names
                 (let ((file-refs (find-refs-in-tree refs-tree symbol-name file-path)))
                   (setf locations (nconc locations file-refs)))))
             ctx:symbol-refs)
    locations))

(defun find-refs-in-tree (refs-tree symbol-name file-path)
  "Find all references to SYMBOL-NAME in an interval tree.
Returns a list of LSP Location dicts."
  (let ((results '()))
    ;; The interval tree stores clef-interval structs with symbol-reference data
    ;; We need to walk all intervals and check symbol names
    (handler-case
        (let ((all-intervals (get-all-intervals-from-tree refs-tree)))
          (dolist (interval all-intervals)
            (let ((ref (clef-symbols::clef-interval-data interval)))
              (when (and ref
                         (string= (clef-symbols:symbol-reference-symbol-name ref)
                                  symbol-name))
                (push (symbol-reference-to-location ref file-path) results)))))
      (error (e)
        (slog :debug "Error searching refs tree: ~A" e)))
    results))

(defun get-all-intervals-from-tree (tree)
  "Extract all intervals from an interval tree.
This is a workaround since cl-interval doesn't expose a direct iteration method."
  ;; Use a very wide range to find all intervals
  ;; interval:find-all takes (tree interval) where interval can be (start . end)
  (handler-case
      (interval:find-all tree (cons 0 most-positive-fixnum))
    (error () '())))

(defun symbol-reference-to-location (ref file-path)
  "Convert a symbol-reference struct to an LSP Location dict."
  (let ((node (clef-symbols:symbol-reference-node ref)))
    (dict "uri" (format nil "file://~A" file-path)
          "range" (node-to-lsp-range node))))

(defun symbol-definition-to-location (def)
  "Convert a symbol-definition struct to an LSP Location dict."
  (let* ((location (clef-symbols:symbol-definition-location def))
         (file-path (clef-symbols:location-file-path location))
         (node (clef-symbols:symbol-definition-node def)))
    (dict "uri" (format nil "file://~A" file-path)
          "range" (node-to-lsp-range node))))
