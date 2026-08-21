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

    (multiple-value-bind (ref-name ref-scope ref-package)
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

        ;; Resolve what the symbol at point actually names, then ask for
        ;; references to THAT, not to everything sharing its spelling.
        (let* ((definition (or definition-at-point
                               (search-up-for-symbol-def ref-scope symbol-name ref-package)))
               (lexical (and definition
                             (lexical-binding-scope-p
                              (clef-symbols:symbol-definition-defining-scope definition))))
               (all-references
                 (if lexical
                     (find-references-to-binding definition symbol-name)
                     (find-all-symbol-references symbol-name))))
          (slog :debug "[textDocument/references] ~A binding, found ~A reference(s)"
                (if lexical "lexical" "top-level") (length all-references))

          ;; Optionally include the definition
          (when include-declaration
            (when (and definition (clef-symbols:symbol-definition-location definition))
              (push (symbol-definition-to-location definition) all-references)))

          ;; Convert to LSP Location array
          (let ((unique (dedupe-locations all-references)))
            (if unique
                (coerce unique 'vector)
                #())))))))

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

;;; ---------------------------------------------------------------------------
;;; Resolving references to a binding, rather than matching a name
;;;
;;; Matching by name alone answered "where else does this word appear", which is
;;; not the question. Asking for references to the `area' bound by
;;; (let ((area ...))) returned a defclass slot called `area', a shadowing FLET
;;; parameter called `area', and -- at workspace scale -- every same-named
;;; binding in every other file: 73 results across 16 files for a binding whose
;;; scope was three lines.
;;;
;;; The machinery to do better was already designed. LEXICAL-SCOPE has a
;;; SYMBOL-REFERENCES slot intended for exactly this, but the code filling it
;;; pushed onto a LET variable instead of onto the hash-table entry, so it was a
;;; no-op, and nothing ever read the slot. Rather than revive a per-scope cache,
;;; resolve each candidate up its own scope chain and compare identity -- the
;;; same path go-to-definition already takes, so the two cannot disagree.
;;;
;;; See docs/surveys/lsp-review.md §1.2.
;;; ---------------------------------------------------------------------------

(defun lexical-binding-scope-p (scope)
  "Is SCOPE a binding form, rather than a whole file or the workspace?

A lexical binding's references are bounded by its scope. A top-level definition's
genuinely are workspace-wide, so those keep the name-matching path."
  (and scope
       (member (clef-symbols:lexical-scope-kind scope)
               '(:let :flet :labels :lambda :defun :defmacro))
       t))

(defun binding-of (ref)
  "The definition REF actually refers to, resolved up REF's own scope chain."
  (search-up-for-symbol-def (clef-symbols:symbol-reference-usage-scope ref)
                            (clef-symbols:symbol-reference-symbol-name ref)
                            (clef-symbols:symbol-reference-package-name ref)))

(defun find-references-to-binding (definition symbol-name)
  "Locations of every reference that resolves to DEFINITION.

Resolving each candidate independently is what makes shadowing correct: an inner
parameter of the same name resolves to a different definition and drops out,
without needing any special-case knowledge of what shadows what."
  (let ((locations '()))
    (maphash (lambda (file-path refs-tree)
               (when refs-tree
                 (dolist (interval (get-all-intervals-from-tree refs-tree))
                   (let ((ref (clef-symbols::clef-interval-data interval)))
                     (when (and ref
                                (string= (clef-symbols:symbol-reference-symbol-name ref)
                                         symbol-name)
                                (eq (binding-of ref) definition))
                       (push (symbol-reference-to-location ref file-path) locations))))))
             ctx:symbol-refs)
    locations))

(defun dedupe-locations (locations)
  "Remove locations naming the same range of the same file.

The declaration is reported twice without this: once because it is in the
reference index like any other occurrence of the symbol, and again because
includeDeclaration pushes it explicitly."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (loc locations)
      (let* ((range (gethash "range" loc))
             (start (and range (gethash "start" range)))
             (end (and range (gethash "end" range)))
             (key (list (gethash "uri" loc)
                        (and start (gethash "line" start))
                        (and start (gethash "character" start))
                        (and end (gethash "line" end))
                        (and end (gethash "character" end)))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push loc result))))
    (nreverse result)))

(defun find-all-symbol-references (symbol-name)
  "Find all references to SYMBOL-NAME across all files in the workspace.
Returns a list of LSP Location dicts.

Name-based, and correct only for top-level definitions. Lexical bindings go
through FIND-REFERENCES-TO-BINDING instead."
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
          "range" (node-to-range node))))

(defun symbol-definition-to-location (def)
  "Convert a symbol-definition struct to an LSP Location dict."
  (let* ((location (clef-symbols:symbol-definition-location def))
         (file-path (clef-symbols:location-file-path location))
         (node (clef-symbols:symbol-definition-node def)))
    (dict "uri" (format nil "file://~A" file-path)
          "range" (node-to-range node))))
