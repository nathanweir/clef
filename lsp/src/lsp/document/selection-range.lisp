(in-package :clef-lsp/document)

;;;; textDocument/selectionRange -- "expand selection".
;;;;
;;;; The client sends positions and gets back, for each, a chain of nested
;;;; ranges: innermost first, each carrying its parent. Pressing the expand key
;;;; repeatedly walks outward along that chain.
;;;;
;;;; This is the most natural fit in the whole protocol for a Lisp. Expanding by
;;;; s-expression is the classic Lisp editing gesture, and a chain of enclosing
;;;; ranges is exactly the tree-sitter ancestry of the node under the cursor --
;;;; no analysis required, just the path from the root down.

(defun node-path-to-position (root line character)
  "Nodes from ROOT down to the innermost one containing the position.

Outermost first. Only nodes that actually contain the position are kept, so the
result is a straight line down the tree rather than a traversal."
  (let ((path '()))
    (labels ((descend (node)
               (when (node-contains-position-p node line character)
                 (push node path)
                 (dolist (child (ts:node-children node))
                   (when (node-contains-position-p child line character)
                     (descend child)
                     (return))))))
      (descend root))
    (nreverse path)))

(defun selection-range-chain (nodes)
  "A nested SelectionRange for NODES, given outermost first.

Each entry carries its enclosing one as `parent', so the client can walk out
without asking again. The outermost has no parent, and the key is omitted rather
than set to null -- the spec makes it optional, and a null parent reads as \"the
chain continues\" to some clients."
  (let ((chain nil)
        (seen nil))
    (dolist (node nodes chain)
      ;; Compared on the node's own coordinates, not on the Range dicts. EQUAL
      ;; over two distinct hash tables is false however identical their contents,
      ;; so comparing the dicts deduplicated nothing -- and the grammar wraps: a
      ;; :LIST-LIT holds a :DEFUN over exactly the same text, which showed up as
      ;; an expand step that visibly selected nothing.
      (let ((extent (list (clef-parser/parser:node-start-point-row node)
                          (clef-parser/parser:node-start-point-column node)
                          (clef-parser/parser:node-end-point-row node)
                          (clef-parser/parser:node-end-point-column node))))
        (unless (equal extent seen)
          (setf seen extent)
          (setf chain (let ((entry (dict "range" (node-to-range node))))
                        (when chain (setf (gethash "parent" entry) chain))
                        entry)))))))

(defun handle-text-document-selection-range (message)
  "Handle a textDocument/selectionRange request.

Answers one chain per requested position, in the same order. A position with no
node under it still gets an entry -- the array is positional, and dropping one
would silently misalign every chain after it."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (positions (href params "positions"))
         (text (gethash document-uri ctx:documents)))
    (slog :debug "[textDocument/selectionRange] Document: ~A" document-uri)
    (if (or (null text) (null positions))
        #()
        (let ((tree (clef-parser/parser:parse-string text))
              (results '()))
          (map nil
               (lambda (position)
                 (let* ((line (href position "line"))
                        (character (href position "character"))
                        (path (node-path-to-position tree line character))
                        (chain (selection-range-chain path)))
                   (push (or chain
                             ;; Nothing under the cursor: a zero-width range at
                             ;; the position itself, which is a valid chain of
                             ;; one and keeps the array aligned.
                             (dict "range" (make-range line character line character)))
                         results)))
               positions)
          (coerce (nreverse results) 'vector)))))
