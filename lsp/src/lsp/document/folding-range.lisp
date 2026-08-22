(in-package :clef-lsp/document)

;;;; textDocument/foldingRange.
;;;;
;;;; What can be collapsed. In a bracket language this is the one LSP method
;;;; that is genuinely easier than in most others: every foldable region is a
;;;; multi-line form, and the tree already has them all.
;;;;
;;;; Two kinds are emitted:
;;;;
;;;;   regions   every multi-line list, vector or DEFUN-shaped form
;;;;   comments  runs of adjacent comment lines
;;;;
;;;; Nesting is left to the client. LSP does not ask for a hierarchy here -- a
;;;; flat list of ranges is the protocol, and editors work out containment from
;;;; the line numbers themselves.

(defparameter +foldable-node-kinds+
  '(:list-lit :vec-lit :map-lit :set-lit :defun :loop-macro
    :read-cond-lit :splicing-read-cond-lit)
  "Node kinds worth offering as a fold.

Atoms cannot span lines and strings should not be folded away -- a multi-line
docstring collapsing to nothing is a worse reading experience than leaving it.")

(defun node-line-span (node)
  "(start-row . end-row) for NODE."
  (cons (clef-parser/parser:node-start-point-row node)
        (clef-parser/parser:node-end-point-row node)))

(defun collect-foldable-spans (node)
  "Every multi-line span under NODE that is worth folding.

Deduplicated, because the grammar wraps: a top-level (defun ...) is a :LIST-LIT
containing a :DEFUN over exactly the same text, and offering the client the same
fold twice is noise."
  (let ((seen (make-hash-table :test 'equal))
        (spans '()))
    (labels ((walk (n)
               (when n
                 (when (member (clef-symbols:node-kind-of n) +foldable-node-kinds+)
                   (let ((span (node-line-span n)))
                     (when (and (> (cdr span) (car span))
                                (not (gethash span seen)))
                       (setf (gethash span seen) t)
                       (push span spans))))
                 (dolist (child (ts:node-children n))
                   (walk child)))))
      (walk node))
    (nreverse spans)))

(defun collect-comment-spans (node)
  "Runs of adjacent comment lines, as (start-row . end-row).

A single comment line is not a fold -- collapsing one line to one line does
nothing, and clients render the affordance anyway."
  (let ((rows '()))
    (labels ((walk (n)
               (when n
                 (when (eq (clef-symbols:node-kind-of n) :comment)
                   (push (clef-parser/parser:node-start-point-row n) rows))
                 (dolist (child (ts:node-children n))
                   (walk child)))))
      (walk node))
    (let ((sorted (sort (remove-duplicates rows) #'<))
          (spans '())
          (run-start nil)
          (previous nil))
      (dolist (row sorted)
        (cond ((null run-start) (setf run-start row previous row))
              ((= row (1+ previous)) (setf previous row))
              (t (when (> previous run-start) (push (cons run-start previous) spans))
                 (setf run-start row previous row))))
      (when (and run-start (> previous run-start))
        (push (cons run-start previous) spans))
      (nreverse spans))))

(defun folding-range (span &optional kind)
  (let ((entry (dict "startLine" (car span) "endLine" (cdr span))))
    (when kind (setf (gethash "kind" entry) kind))
    entry))

(defun handle-text-document-folding-range (message)
  "Handle a textDocument/foldingRange request."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (text (gethash document-uri ctx:documents)))
    (slog :debug "[textDocument/foldingRange] Document: ~A" document-uri)
    (if (null text)
        #()
        (let* ((tree (clef-parser/parser:parse-string text))
               (ranges (append (mapcar #'folding-range (collect-foldable-spans tree))
                               (mapcar (lambda (span) (folding-range span "comment"))
                                       (collect-comment-spans tree)))))
          (slog :debug "[textDocument/foldingRange] ~A range(s)" (length ranges))
          (coerce ranges 'vector)))))
