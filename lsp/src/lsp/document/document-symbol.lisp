(in-package :clef-lsp/document)

;;;; textDocument/documentSymbol -- the file outline.
;;;;
;;;; The single most-used navigation request there is: the symbol pane, the
;;;; breadcrumb bar, in-file fuzzy jump, and the structural view every agent
;;;; asks for before reading a file. clef advertised none of it, so a client
;;;; calling it got "Method not found" -- including Claude Code's own LSP
;;;; client, which is how this was noticed. See docs/surveys/lsp-review.md §2.
;;;;
;;;; Returns DocumentSymbol[] rather than SymbolInformation[]. The latter is
;;;; deprecated in 3.17, carries a full Location per entry where a Range will do,
;;;; and cannot express nesting.

(defun document-scope-for (file-path)
  "The :document scope for FILE-PATH -- the one holding its top-level definitions.

Every scope for the file is in one interval tree, and the document scope is the
one spanning the whole file. Found by kind rather than by span, since a file
containing a single top-level form has a defun scope of nearly the same extent."
  (let ((tree (gethash file-path ctx:lexical-scopes)))
    (when tree
      (dolist (interval (get-all-intervals-from-tree tree))
        (let ((scope (clef-symbols::clef-interval-data interval)))
          (when (and scope (eq (clef-symbols:lexical-scope-kind scope) :document))
            (return scope)))))))

(defun trim-range-start (range shift)
  "RANGE with its start moved SHIFT characters right.

For a DEFPACKAGE name the node spans a marker -- the `:' of :foo, the `#:' of
#:foo -- that the reported name does not include. Without this the
DocumentSymbol's `name' and its `selectionRange' describe different text, which
the corpus sweep flags as `selectionRange text /= name'."
  (if (and shift (plusp shift))
      (let* ((start (gethash "start" range))
             (moved (dict "line" (gethash "line" start)
                          "character" (+ (gethash "character" start) shift))))
        (dict "start" moved "end" (gethash "end" range)))
      range))

(defun definition-to-document-symbol (def)
  "One DocumentSymbol for DEF, or NIL if it cannot be located.

The enclosing form comes from the definition's FORM-NODE, recorded at index
time. An earlier version recovered it by stabbing the scope interval tree at the
name's offset, which failed for any file whose single top-level DEFUN spans the
whole file: that scope's extent is identical to the document scope's, and the
tree keeps only one of two identical intervals. See
docs/surveys/lsp-review.md §1.8."
  (let ((name (clef-symbols:symbol-definition-symbol-name def))
        (name-node (clef-symbols:symbol-definition-node def))
        (form-node (clef-symbols:symbol-definition-form-node def)))
    (when (and name name-node)
      (let* ((selection-range (trim-range-start
                               (node-to-range name-node)
                               (clef-symbols:symbol-definition-name-start-shift def)))
             ;; The spec requires selectionRange to be contained in range.
             ;; Falling back to the name for both satisfies that trivially and
             ;; still navigates correctly -- it only gives up the breadcrumb.
             (range (if form-node (node-to-range form-node) selection-range)))
        (dict "name" name
              "kind" (lisp-kind-to-lsp-kind (clef-symbols:symbol-definition-kind def))
              "range" range
              "selectionRange" selection-range)))))

(defun handle-text-document-document-symbol (message)
  "Handle a textDocument/documentSymbol request.

Reports this file's top-level definitions. Nested bindings -- parameters, LET
variables -- are deliberately excluded: an outline listing every local would be
unreadable, and no editor presents them that way."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (file-path (clef-util:cleanup-path document-uri)))
    (slog :debug "[textDocument/documentSymbol] Document: ~A" document-uri)
    (let ((scope (document-scope-for file-path)))
      (if (null scope)
          ;; An empty array, not NIL. "This file has no symbols" is a real
          ;; answer; the client asked a question and is owed one.
          #()
          (let ((symbols (loop for def in (reverse (clef-symbols:lexical-scope-symbol-definitions scope))
                               for entry = (definition-to-document-symbol def)
                               when entry collect entry)))
            (slog :debug "[textDocument/documentSymbol] ~A symbol(s)" (length symbols))
            (coerce symbols 'vector))))))
