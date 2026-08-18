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

             (multiple-value-bind (ref-name ref-scope)
                                  (get-ref-for-doc-pos document-uri line character)
                                  (slog :debug "[textDocument/definition] Reference name: ~A" ref-name)
                                  (when (not ref-scope)
                                        (slog :warn "[textDocument/definition] Could not find scope for position.")
                                        (return-from handle-text-document-definition
                                                     (make-goto-definition-response nil)))
                                  (make-goto-definition-response
                                    (search-up-for-symbol-def ref-scope ref-name)))))


(defun search-up-for-symbol-def (ref-scope ref-name)
       "Search up the lexical scope tree from REF-SCOPE to find the definition of REF-NAME.
If not found in the scope tree, falls back to searching the workspace symbol index
for cross-file definitions."
       ;; Base case: reached end of scope tree, try workspace index
       (unless ref-scope
               (slog :debug "Scope tree exhausted, searching workspace index for ~A" ref-name)
               (return-from search-up-for-symbol-def
                            (search-workspace-index-for-symbol ref-name)))
       (slog :debug "searching up for ref name ~A with scope kind ~A" ref-name
             (lexical-scope-kind ref-scope))
       ;; Check each symbol definition in the scope. If it has the same name as ref-name, return
       ;; that definition. Otherwise, recurse on this function with the parent scope
       (let ((defs (lexical-scope-symbol-definitions ref-scope)))
            (dolist (def defs)
                    (when (string= (symbol-definition-symbol-name def) ref-name)
                          (slog :debug "Found symbol definition for ~A in scope" ref-name)
                          (return-from search-up-for-symbol-def def))))
       (let ((parent-scope (lexical-scope-parent-scope ref-scope)))
            (search-up-for-symbol-def parent-scope ref-name)))

(defun search-workspace-index-for-symbol (symbol-name)
       "Search the workspace symbol index for a symbol definition.
Returns the first match, or nil if not found."
       (let ((defs (clef-symbols:lookup-in-workspace-index symbol-name)))
            (when defs
                  (slog :debug "Found ~A definition(s) for ~A in workspace index"
                        (length defs) symbol-name)
                  ;; Return the first match (could be enhanced to handle multiple definitions)
                  (first defs))))

;; TODO: This will definitely be used elsewhere
(defun node-to-lsp-range (node)
       "Convert byte offsets to an LSP Range dict."
       (multiple-value-bind (start-line start-char end-line end-char)
                            (clef-parser/parser:node-range node)
                            (dict "start" (dict "line" start-line
                                                "character" start-char)
                                  "end" (dict "line" end-line
                                              "character" end-char))))

(defun make-goto-definition-response (symbol-def)
       (unless symbol-def
               (slog :warn "[make-goto-definition-response] No symbol definition provided.")
               ;; TODO: I'm not 100% sure this is valid; do I need to do #() to serialize to nil?
               (return-from make-goto-definition-response #()))
       ;; Put in dummy values for now
       (let* ((scope (symbol-definition-defining-scope symbol-def))
              (file-path (location-file-path
                           (symbol-definition-location symbol-def)))
              (uri (clef-util:path-to-file-uri file-path)))
             (declare (ignorable scope))

             ;; Definitions inside SBCL itself resolve to a logical pathname we
             ;; cannot turn into a real file URI. Report "no definition" rather
             ;; than handing the editor a location it cannot open.
             (if uri
                 ;; Just return one Location for now
                 (dict "uri" uri
                       "range" (node-to-lsp-range
                                 (symbol-definition-node symbol-def)))
                 (progn
                  (slog :debug "[make-goto-definition-response] No usable file URI for ~A"
                        file-path)
                  #()))))
;; TODO: Attempting to return this LocationLink did not work. Revisit.
;; (dict "targetUri" (format nil "file://~A" (location-file-path
;;                                             (symbol-definition-location symbol-def)))
;;       "targetRange" (node-to-lsp-range (lexical-scope-node scope))
;;       "targetSelectionRange" (node-to-lsp-range (symbol-definition-node symbol-def)))))


;; "targetRange" (byte-offsets-to-lsp-range
;;                 file-path
;;                 (location-start
;;                   (lexical-scope-location scope))
;;                 (location-end
;;                   (lexical-scope-location scope)))

;; "targetSelectionRange" (byte-offsets-to-lsp-range
;;                          file-path
;;                          (location-start
;;                            (symbol-definition-location symbol-def))
;;                          (location-end
;;                            (symbol-definition-location symbol-def))))))


;;       interface LocationLink {

;; /**
;;  * Span of the origin of this link.
;;  *
;;  * Used as the underlined span for mouse interaction. Defaults to the word
;;  * range at the mouse position.
;;  */
;; originSelectionRange?: Range;

;; /**
;;  * The target resource identifier of this link.
;;  */
;; targetUri: DocumentUri;

;; /**
;;  * The full target range of this link. If the target for example is a symbol
;;  * then target range is the range enclosing this symbol not including
;;  * leading/trailing whitespace but everything else like comments. This
;;  * information is typically used to highlight the range in the editor.
;;  */
;; targetRange: Range;

;; /**
;;  * The range that should be selected and revealed when this link is being
;;  * followed, e.g the name of a function. Must be contained by the
;;  * `targetRange`. See also `DocumentSymbol#range`
;;  */
;; targetSelectionRange: Range;
;;       }
