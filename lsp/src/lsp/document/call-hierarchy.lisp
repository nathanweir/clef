(in-package :clef-lsp/document)

;;;; Call hierarchy: who calls this, and what does this call.
;;;;
;;;; Three methods that work as a set. PREPARE-CALL-HIERARCHY turns a position
;;;; into an item; the client then asks that item for its incoming or outgoing
;;;; calls, and may keep walking. Claude Code's LSP client exposes all three and
;;;; got "Method not found" for each. See docs/surveys/lsp-review.md §2.
;;;;
;;;; This is built on FORM-NODE -- the whole-definition node recorded on each
;;;; symbol-definition. "Which function is this call inside?" is answered by
;;;; finding the top-level definition whose form contains the reference, which is
;;;; only possible because that node is kept.
;;;;
;;;; Honest limits, stated here rather than discovered later:
;;;;
;;;; - Resolution is by NAME against the workspace index, so two definitions of
;;;;   the same name in different packages are not told apart. The index is keyed
;;;;   by name alone; fixing it properly is a change to the index, not to this.
;;;; - A call through FUNCALL, APPLY or a variable holding a function is not a
;;;;   textual reference to the callee and will not be seen.
;;;; - Anything a macro expands into is invisible, as everywhere else in a
;;;;   tree-sitter-first design.

(defun node-contains-position-p (node line character)
  "Is (LINE, CHARACTER) inside NODE's extent?"
  (and node
       (let ((start-row (clef-parser/parser:node-start-point-row node))
             (start-col (clef-parser/parser:node-start-point-column node))
             (end-row (clef-parser/parser:node-end-point-row node))
             (end-col (clef-parser/parser:node-end-point-column node)))
         (position-in-range-p line character start-row start-col end-row end-col))))

(defun node-start-position (node)
  (list (clef-parser/parser:node-start-point-row node)
        (clef-parser/parser:node-start-point-column node)))

(defun top-level-definitions (file-path)
  "Every top-level definition recorded for FILE-PATH."
  (let ((scope (document-scope-for file-path)))
    (when scope
      (clef-symbols:lexical-scope-symbol-definitions scope))))

(defun definition-containing-position (file-path line character)
  "The top-level definition whose form contains the position, or NIL.

This is what makes \"who calls this\" answerable: a reference is attributed to
the definition it sits inside."
  (dolist (def (top-level-definitions file-path))
    (let ((form (clef-symbols:symbol-definition-form-node def)))
      (when (node-contains-position-p form line character)
        (return def)))))

(defun definition-to-hierarchy-item (def)
  "A CallHierarchyItem for DEF, or NIL if it cannot be located."
  (let* ((name (clef-symbols:symbol-definition-symbol-name def))
         (name-node (clef-symbols:symbol-definition-node def))
         (form-node (clef-symbols:symbol-definition-form-node def))
         (location (clef-symbols:symbol-definition-location def))
         (file-path (when location (clef-symbols:location-file-path location))))
    (when (and name name-node file-path)
      (let ((selection-range (node-to-range name-node)))
        (dict "name" name
              "kind" (lisp-kind-to-lsp-kind (clef-symbols:symbol-definition-kind def))
              "uri" (format nil "file://~A" file-path)
              "range" (if form-node (node-to-range form-node) selection-range)
              "selectionRange" selection-range)))))

(defun definition-from-item (item)
  "Recover the definition a CallHierarchyItem refers to.

Matched on name and file. The item came from DEFINITION-TO-HIERARCHY-ITEM, so
the name is exact; the file disambiguates same-named definitions across the
workspace, as far as the name-keyed index allows."
  (let* ((name (href item "name"))
         (uri (href item "uri"))
         (file-path (clef-util:cleanup-path uri)))
    (find-if (lambda (def)
               (let ((location (clef-symbols:symbol-definition-location def)))
                 (and location
                      (string= (clef-symbols:location-file-path location) file-path))))
             (clef-symbols:lookup-in-workspace-index name))))

;;; ---------------------------------------------------------------------------
;;; textDocument/prepareCallHierarchy
;;; ---------------------------------------------------------------------------

(defun handle-text-document-prepare-call-hierarchy (message)
  "Turn a position into a CallHierarchyItem.

Works whether the cursor is on a call or on the definition itself: a reference
resolves through the scope chain, and failing that the enclosing definition is
used, which is what makes invoking this from inside a function body do the
obvious thing."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (file-path (clef-util:cleanup-path document-uri)))
    (slog :debug "[prepareCallHierarchy] ~A ~A:~A" document-uri line character)
    (multiple-value-bind (ref-name ref-scope ref-package)
        (get-ref-for-doc-pos document-uri line character)
      (let* ((def (cond
                    ;; On a call or other reference: resolve what it names.
                    (ref-name (search-up-for-symbol-def ref-scope ref-name ref-package))
                    ;; On the name in a definition.
                    (t (find-definition-at-position document-uri line character))))
             ;; Anywhere else inside a definition: use the definition itself.
             (def (or def (definition-containing-position file-path line character)))
             (item (when def (definition-to-hierarchy-item def))))
        (if item (vector item) #())))))

;;; ---------------------------------------------------------------------------
;;; callHierarchy/incomingCalls
;;; ---------------------------------------------------------------------------

(defun handle-call-hierarchy-incoming-calls (message)
  "Who calls this?

Every reference to the name, attributed to the definition it sits inside, then
grouped so each caller appears once with all of its call sites."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (item (href params "item"))
         (name (and item (href item "name")))
         (target (and item (definition-from-item item))))
    (slog :debug "[incomingCalls] ~A" name)
    (if (null name)
        #()
        (let ((callers (make-hash-table :test 'equal)))
          (maphash
           (lambda (file-path refs-tree)
             (when refs-tree
               (dolist (interval (get-all-intervals-from-tree refs-tree))
                 (let ((ref (clef-symbols::clef-interval-data interval)))
                   (when (and ref
                              (string= (clef-symbols:symbol-reference-symbol-name ref) name))
                     (let* ((node (clef-symbols:symbol-reference-node ref))
                            (start (and node (node-start-position node)))
                            (caller (and start
                                         (definition-containing-position
                                          file-path (first start) (second start)))))
                       ;; A definition's own name node sits inside its own form,
                       ;; so it would otherwise report the function as calling
                       ;; itself once, spuriously.
                       (when (and caller
                                  (not (and target (eq caller target)))
                                  node)
                         (push (node-to-range node)
                               (gethash caller callers)))))))))
           ctx:symbol-refs)
          (let ((results '()))
            (maphash (lambda (caller ranges)
                       (let ((from (definition-to-hierarchy-item caller)))
                         (when from
                           (push (dict "from" from
                                       "fromRanges" (coerce (nreverse ranges) 'vector))
                                 results))))
                     callers)
            (coerce (nreverse results) 'vector))))))

;;; ---------------------------------------------------------------------------
;;; callHierarchy/outgoingCalls
;;; ---------------------------------------------------------------------------

(defun symbol-nodes-under (node)
  "Every :SYM-LIT node under NODE."
  (let ((found '()))
    (labels ((walk (n)
               (when n
                 (when (eq (clef-symbols:node-kind-of n) :sym-lit)
                   (push n found))
                 (dolist (child (ts:node-children n))
                   (walk child)))))
      (walk node))
    (nreverse found)))

(defun handle-call-hierarchy-outgoing-calls (message)
  "What does this call?

Every symbol inside the definition's form that names something the workspace
index knows about. Bounded by the form, so a definition's callees never leak in
from its neighbours."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (item (href params "item"))
         (def (and item (definition-from-item item)))
         (form-node (and def (clef-symbols:symbol-definition-form-node def)))
         (location (and def (clef-symbols:symbol-definition-location def)))
         (file-path (and location (clef-symbols:location-file-path location)))
         (source (when file-path
                   (or (gethash (format nil "file://~A" file-path) ctx:documents)
                       (ignore-errors (uiop:read-file-string file-path))))))
    (slog :debug "[outgoingCalls] ~A" (and item (href item "name")))
    (if (or (null form-node) (null source))
        #()
        (let ((callees (make-hash-table :test 'equal))
              (self (clef-symbols:symbol-definition-symbol-name def)))
          (dolist (node (symbol-nodes-under form-node))
            (let ((text (ignore-errors
                         (clef-parser/parser:node-text node source))))
              (when (and text (not (string= text self)))
                (let ((target (first (clef-symbols:lookup-in-workspace-index text))))
                  (when target
                    (push (node-to-range node) (gethash target callees)))))))
          (let ((results '()))
            (maphash (lambda (target ranges)
                       (let ((to (definition-to-hierarchy-item target)))
                         (when to
                           (push (dict "to" to
                                       "fromRanges" (coerce (nreverse ranges) 'vector))
                                 results))))
                     callees)
            (coerce (nreverse results) 'vector))))))
