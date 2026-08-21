(in-package :clef-lsp/document)

;; DocumentHighlightKind constants (LSP spec)
(defconstant +highlight-kind-text+ 1 "A textual occurrence.")
(defconstant +highlight-kind-read+ 2 "Read-access of a symbol, like reading a variable.")
(defconstant +highlight-kind-write+ 3 "Write-access of a symbol, like writing to a variable.")

(defun dedupe-highlights (highlights)
  "Drop highlights covering the same range.

The name node of a binding is recorded both as a definition and as a reference,
so without this it is highlighted twice -- and with two different kinds, which
some clients render as an overlapping mess."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (h highlights)
      (let* ((range (gethash "range" h))
             (start (gethash "start" range))
             (end (gethash "end" range))
             (key (list (gethash "line" start) (gethash "character" start)
                        (gethash "line" end) (gethash "character" end))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push h result))))
    (nreverse result)))

(defun handle-text-document-highlight (message)
  "Handle a textDocument/documentHighlight request.

Every occurrence *of the binding* under the cursor, within this file.

Resolution goes through the same path as find-references and go-to-definition,
which is the point: highlighting used to match by name, so putting the cursor on
a LET-bound variable lit up every same-named symbol in the file including a
DEFCLASS slot and a shadowing FLET parameter. Sharing the resolution means the
three cannot disagree about what a symbol refers to."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (file-path (clef-util:cleanup-path document-uri)))
    (slog :debug "[textDocument/documentHighlight] Document: ~A" document-uri)

    (multiple-value-bind (ref-name ref-scope ref-package)
        (get-ref-for-doc-pos document-uri line character)
      (let* ((definition (or (when ref-name
                               (search-up-for-symbol-def ref-scope ref-name ref-package))
                             (find-definition-at-position document-uri line character)))
             (symbol-name (or ref-name
                              (when definition
                                (clef-symbols:symbol-definition-symbol-name definition)))))
        (unless symbol-name
          (slog :debug "[textDocument/documentHighlight] No symbol at position")
          (return-from handle-text-document-highlight #()))

        (let* ((lexical (and definition
                             (lexical-binding-scope-p
                              (clef-symbols:symbol-definition-defining-scope definition))))
               (highlights '()))
          ;; Uses, from this file only -- documentHighlight is per-document.
          (let ((refs-tree (gethash file-path ctx:symbol-refs)))
            (when refs-tree
              (dolist (interval (get-all-intervals-from-tree refs-tree))
                (let ((ref (clef-symbols::clef-interval-data interval)))
                  (when (and ref
                             (string= (clef-symbols:symbol-reference-symbol-name ref)
                                      symbol-name)
                             ;; A lexical binding's occurrences are only those
                             ;; that actually resolve to it. A top-level name
                             ;; keeps the name match, which is right for it.
                             (or (not lexical)
                                 (eq (binding-of ref file-path) definition)))
                    (push (make-highlight (clef-symbols:symbol-reference-node ref)
                                          +highlight-kind-read+)
                          highlights))))))

          ;; The binding itself, when it lives in this file.
          (when definition
            (let ((node (clef-symbols:symbol-definition-node definition))
                  (location (clef-symbols:symbol-definition-location definition)))
              (when (and node location
                         (string= (clef-symbols:location-file-path location) file-path))
                (push (make-highlight node +highlight-kind-write+) highlights))))

          (let ((unique (dedupe-highlights (nreverse highlights))))
            (slog :debug "[textDocument/documentHighlight] Found ~A highlight(s)"
                  (length unique))
            (coerce unique 'vector)))))))

(defun make-highlight (node kind)
  "Create an LSP DocumentHighlight dict from a tree-sitter node."
  (dict "range" (node-to-range node)
        "kind" kind))
