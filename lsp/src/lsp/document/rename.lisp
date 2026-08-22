(in-package :clef-lsp/document)

;;;; textDocument/rename and textDocument/prepareRename.
;;;;
;;;; prepareRename is the pre-flight. The editor sends it the moment the user
;;;; presses the rename key, before anything has been typed, and it answers two
;;;; questions: may this be renamed at all, and what exactly will be replaced.
;;;; Returning null makes the editor refuse immediately, which is far better
;;;; than accepting a new name and then doing nothing with it.
;;;;
;;;; The second question is not idle in Common Lisp. A qualified reference is
;;;; written `clef-jsonrpc/types:request-params', and only the name half is the
;;;; symbol -- the grammar splits it, so the recorded reference covers exactly
;;;; that half and the package prefix survives a rename untouched. prepareRename
;;;; is where the editor is told which characters those are.
;;;;
;;;; Both are built on RESOLVE-SYMBOL-AT and LOCATIONS-FOR-SYMBOL, the same
;;;; functions find-references uses. That is deliberate and load-bearing: a
;;;; rename that edits a different set than find-references reports is a
;;;; data-loss bug. Sharing the resolution makes disagreement impossible rather
;;;; than unlikely.

(defun renameable-p (definition)
  "May this binding be renamed?

**No, for anything clef did not find in the workspace.** Resolution walks into
the global scope, which holds an entry for every Common Lisp symbol and every
symbol of every loaded library. Renaming LIST would rewrite every call site in
the project while leaving the actual function untouched -- a silent, wholesale
corruption, and exactly the kind of thing a rename must refuse rather than
attempt.

The test is the defining scope: anything from :WORKSPACE came from the image,
not from a file we can edit."
  (and definition
       (clef-symbols:symbol-definition-location definition)
       (let ((scope (clef-symbols:symbol-definition-defining-scope definition)))
         (and scope
              (not (eq (clef-symbols:lexical-scope-kind scope) :workspace))))
       t))

(defun symbol-node-at (document-uri line character)
  "The tree-sitter node for the symbol under the cursor, or NIL.

Needed for the range prepareRename reports. For a qualified reference this is
the name half only, which is what makes renaming leave the package prefix
alone."
  (let* ((file-path (clef-util:cleanup-path document-uri))
         (offset (ignore-errors
                  (clef-symbols:line-char-to-byte-offset file-path line character)))
         (refs (when offset
                 (ignore-errors
                  (interval:find-all (gethash file-path ctx:symbol-refs) offset)))))
    (cond
      (refs (clef-symbols:symbol-reference-node
             (clef-symbols::clef-interval-data (first refs))))
      (t (let ((def (find-definition-at-position document-uri line character)))
           (when def (clef-symbols:symbol-definition-node def)))))))

(defun handle-text-document-prepare-rename (message)
  "Handle a textDocument/prepareRename request.

Returns the range that will be replaced and a placeholder for the input box, or
NIL when the position cannot be renamed -- which the editor turns into a refusal
before the user has typed anything."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character")))
    (slog :debug "[textDocument/prepareRename] ~A ~A:~A" document-uri line character)
    (multiple-value-bind (symbol-name definition lexical)
        (resolve-symbol-at document-uri line character)
      (declare (ignore lexical))
      (let ((node (when symbol-name (symbol-node-at document-uri line character))))
        (cond
          ((null symbol-name)
           (slog :debug "[prepareRename] Nothing to rename here")
           nil)
          ((not (renameable-p definition))
           (slog :debug "[prepareRename] ~A is not defined in this workspace" symbol-name)
           nil)
          ((null node) nil)
          (t (dict "range" (node-to-range node)
                   "placeholder" symbol-name)))))))

(defun rename-edits (locations new-name)
  "Group LOCATIONS into a WorkspaceEdit's `changes' map.

Edits within a file are ordered last-first. Applying them in that order means
each edit's range is still valid when it is applied, because nothing before it
has shifted -- clients are supposed to handle this themselves, and ordering the
list defensively costs nothing."
  (let ((by-uri (make-hash-table :test 'equal)))
    (dolist (location locations)
      (push (dict "range" (gethash "range" location) "newText" new-name)
            (gethash (gethash "uri" location) by-uri)))
    (let ((changes (make-hash-table :test 'equal)))
      (maphash (lambda (uri edits)
                 (setf (gethash uri changes)
                       (coerce (sort edits
                                     (lambda (a b)
                                       (let ((sa (gethash "start" (gethash "range" a)))
                                             (sb (gethash "start" (gethash "range" b))))
                                         (or (> (gethash "line" sa) (gethash "line" sb))
                                             (and (= (gethash "line" sa) (gethash "line" sb))
                                                  (> (gethash "character" sa)
                                                     (gethash "character" sb)))))))
                               'vector)))
               by-uri)
      changes)))

(defun handle-text-document-rename (message)
  "Handle a textDocument/rename request.

Edits exactly the set find-references reports, including the declaration."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (new-name (href params "new-name")))
    (slog :debug "[textDocument/rename] ~A ~A:~A -> ~A"
          document-uri line character new-name)
    (multiple-value-bind (symbol-name definition lexical)
        (resolve-symbol-at document-uri line character)
      (cond
        ((or (null symbol-name) (null new-name) (string= new-name ""))
         (dict "changes" (make-hash-table :test 'equal)))
        ((not (renameable-p definition))
         ;; Refused for the same reason prepareRename refuses. A client that
         ;; skipped the pre-flight must not get a workspace-wide rewrite of a
         ;; standard-library name.
         (slog :warn "[textDocument/rename] Refusing to rename ~A: not defined in this workspace"
               symbol-name)
         (dict "changes" (make-hash-table :test 'equal)))
        (t
         (let ((locations (locations-for-symbol symbol-name definition lexical t)))
           (slog :debug "[textDocument/rename] ~A edit(s)" (length locations))
           (dict "changes" (rename-edits locations new-name))))))))
