(in-package :clef-lsp/document)

;;;; textDocument/implementation.
;;;;
;;;; The LSP method was designed for languages with interfaces and abstract
;;;; methods. Common Lisp's equivalent is exact and arguably cleaner: the
;;;; implementations of a generic function are its methods.
;;;;
;;;;     (defgeneric area (shape))          <- the declaration
;;;;     (defmethod area ((s circle)) ...)  <- an implementation
;;;;     (defmethod area ((s square)) ...)  <- another
;;;;
;;;; Answerable only because the indexer now records which defining form it saw.
;;;; It used to write :FUNCTION for everything DEFUN-shaped under a TODO reading
;;;; "Calc specific kind", which left DEFMETHOD indistinguishable from DEFUN.
;;;;
;;;; Claude Code's LSP client exposes this as goToImplementation and got
;;;; "Method not found". See docs/surveys/lsp-review.md §2.

(defun definition-to-location (def)
  "An LSP Location for DEF, or NIL when it has no usable file."
  (let* ((location (clef-symbols:symbol-definition-location def))
         (file-path (when location (clef-symbols:location-file-path location)))
         (uri (when file-path (clef-util:path-to-file-uri file-path)))
         (node (clef-symbols:symbol-definition-node def)))
    (when (and uri node)
      (dict "uri" uri "range" (node-to-range node)))))

(defun implementations-of (name)
  "Every definition of NAME that is an implementation rather than a declaration.

Methods, when there are any. A name with no methods has no implementations
distinct from itself, and the spec's answer for that is an empty result rather
than pointing back at the declaration -- go-to-definition already does that."
  (remove-if-not (lambda (def)
                   (eq (clef-symbols:symbol-definition-kind def) :method))
                 (clef-symbols:lookup-in-workspace-index name)))

(defun handle-text-document-implementation (message)
  "Handle a textDocument/implementation request.

Returns the methods of the generic function under the cursor. Works from a call
site, from the DEFGENERIC's own name, or from any DEFMETHOD of it -- all three
resolve to the same name, and the name is what the methods are indexed under."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character")))
    (slog :debug "[textDocument/implementation] ~A ~A:~A" document-uri line character)
    (multiple-value-bind (ref-name ref-scope ref-package)
        (get-ref-for-doc-pos document-uri line character)
      (declare (ignore ref-scope ref-package))
      (let* ((name (or ref-name
                       (let ((def (find-definition-at-position document-uri line character)))
                         (when def (clef-symbols:symbol-definition-symbol-name def)))))
             (locations (when name
                          (remove nil (mapcar #'definition-to-location
                                              (implementations-of name))))))
        (slog :debug "[textDocument/implementation] ~A implementation(s) of ~A"
              (length locations) name)
        (coerce locations 'vector)))))
