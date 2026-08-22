(in-package :clef-lsp/document)

;;;; textDocument/codeLens -- a reference count above each definition.
;;;;
;;;; The classic use, and cheap here because the reference index already exists.
;;;; A definition nobody calls is worth noticing, and so is one called from
;;;; thirty places before you change its signature.
;;;;
;;;; **No command attached, deliberately.** A CodeLens may carry a command the
;;;; client runs when the lens is clicked, but there is no standard one for
;;;; "show references" -- `editor.action.showReferences` is VS Code's, and a
;;;; client that does not implement it renders a lens that looks clickable and
;;;; does nothing. A title-only lens is valid per the spec, honest about what it
;;;; offers, and still says the useful thing. Wiring a command is a per-client
;;;; decision rather than something to guess at.

(defun count-workspace-references (name definition file-path)
  "How many references across the workspace resolve to DEFINITION.

Counts the definition's own name node out, since a function is not one of its
own callers -- and every definition would otherwise read as having one more
reference than it does."
  (let ((count 0))
    (maphash
     (lambda (path refs-tree)
       (when refs-tree
         (dolist (interval (get-all-intervals-from-tree refs-tree))
           (let ((ref (clef-symbols::clef-interval-data interval)))
             (when (and ref
                        (string= (clef-symbols:symbol-reference-symbol-name ref) name)
                        ;; The declaration itself is in the reference index too.
                        (not (and (string= path file-path)
                                  (eq (clef-symbols:symbol-reference-node ref)
                                      (clef-symbols:symbol-definition-node definition)))))
               (incf count))))))
     ctx:symbol-refs)
    count))

(defun definition-code-lens (definition file-path)
  "A lens for one top-level definition, or NIL."
  (let ((node (clef-symbols:symbol-definition-node definition))
        (name (clef-symbols:symbol-definition-symbol-name definition)))
    (when (and node name)
      (let ((count (count-workspace-references name definition file-path)))
        (dict "range" (node-to-range node)
              "command" (dict "title" (if (= count 1)
                                          "1 reference"
                                          (format nil "~D references" count))
                              ;; An empty command string means "nothing to run".
                              ;; The title still displays.
                              "command" ""))))))

(defun handle-text-document-code-lens (message)
  "Handle a textDocument/codeLens request.

Top-level definitions only. A lens above every LET binding would bury the file
in annotations, and the count that matters is the one for something other code
can call."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (file-path (clef-util:cleanup-path document-uri)))
    (slog :debug "[textDocument/codeLens] Document: ~A" document-uri)
    (let* ((scope (document-scope-for file-path))
           (lenses (when scope
                     (loop for definition in (reverse
                                              (clef-symbols:lexical-scope-symbol-definitions
                                               scope))
                           for lens = (definition-code-lens definition file-path)
                           when lens collect lens))))
      (slog :debug "[textDocument/codeLens] ~A lens(es)" (length lenses))
      (coerce lenses 'vector))))
