(in-package :clef-lsp/workspace)

;; SymbolKind constants (LSP spec)
(defconstant +symbol-kind-file+ 1)
(defconstant +symbol-kind-module+ 2)
(defconstant +symbol-kind-namespace+ 3)
(defconstant +symbol-kind-package+ 4)
(defconstant +symbol-kind-class+ 5)
(defconstant +symbol-kind-method+ 6)
(defconstant +symbol-kind-property+ 7)
(defconstant +symbol-kind-field+ 8)
(defconstant +symbol-kind-constructor+ 9)
(defconstant +symbol-kind-enum+ 10)
(defconstant +symbol-kind-interface+ 11)
(defconstant +symbol-kind-function+ 12)
(defconstant +symbol-kind-variable+ 13)
(defconstant +symbol-kind-constant+ 14)
(defconstant +symbol-kind-string+ 15)
(defconstant +symbol-kind-number+ 16)
(defconstant +symbol-kind-boolean+ 17)
(defconstant +symbol-kind-array+ 18)
(defconstant +symbol-kind-object+ 19)
(defconstant +symbol-kind-key+ 20)
(defconstant +symbol-kind-null+ 21)
(defconstant +symbol-kind-enum-member+ 22)
(defconstant +symbol-kind-struct+ 23)
(defconstant +symbol-kind-event+ 24)
(defconstant +symbol-kind-operator+ 25)
(defconstant +symbol-kind-type-parameter+ 26)

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
               clef-symbols:*workspace-symbol-index*)

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

(defun lisp-kind-to-lsp-kind (kind)
  "Convert internal symbol kind to LSP SymbolKind."
  (case kind
    (:function +symbol-kind-function+)
    (:macro +symbol-kind-function+)  ; No macro kind in LSP, use function
    (:variable +symbol-kind-variable+)
    (:constant +symbol-kind-constant+)
    (:parameter +symbol-kind-variable+)
    (:class +symbol-kind-class+)
    (:struct +symbol-kind-struct+)
    (:method +symbol-kind-method+)
    (:special-operator +symbol-kind-operator+)
    (otherwise +symbol-kind-variable+)))

(defun node-to-range (node)
  "Convert a tree-sitter node to an LSP Range dict."
  (when node
    (dict "start" (dict "line" (clef-parser/parser:node-start-point-row node)
                        "character" (clef-parser/parser:node-start-point-column node))
          "end" (dict "line" (clef-parser/parser:node-end-point-row node)
                      "character" (clef-parser/parser:node-end-point-column node)))))

;; Register the handler
(setf (gethash "workspace/symbol" clef-lsp/server:*handlers*)
      #'handle-workspace-symbol)
