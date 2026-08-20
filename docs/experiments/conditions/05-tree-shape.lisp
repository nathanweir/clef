;;;; What does a tree-sitter list node's child list actually contain?
;;;;
;;;; To walk SBCL's ORIGINAL-SOURCE-PATH (established in probe 04 to be
;;;; innermost-first, positional, operator-at-index-0) into the tree-sitter tree,
;;;; the Nth element of a form has to be findable. That only works if we know
;;;; whether the parens, comments and reader macros show up as children.
;;;;
;;;; Run: sbcl --script docs/experiments/conditions/05-tree-shape.lisp

(load (merge-pathnames "lsp/load.lisp" (truename ".")))

(defparameter *source* "(defun target ()
  (list 1 2 (no-such-fn) 4))

;; a comment between forms
(defun quoted ()
  '(a b c))

(defun with-comment ()
  (list 1 ;; inline comment
        2))

(defun strings-and-chars ()
  (list \"no-such-fn\" #\\a :kw))
")

(defun kind (node)
  (let ((k (first node)))
    (if (consp k) (second k) k)))

(defun dump (node depth)
  (format t "~v@T~S  range=~S~%" (* 2 depth) (kind node)
          (ignore-errors (cl-tree-sitter:node-range node)))
  (dolist (c (cl-tree-sitter:node-children node))
    (dump c (1+ depth))))

(let ((tree (clef-parser/parser:parse-string *source*)))
  (format t "~&===== full tree =====~%")
  (dump tree 0)

  (format t "~&~%===== children of each top-level form =====~%")
  (let ((forms (remove-if (lambda (n) (eq (kind n) :comment))
                          (cl-tree-sitter:node-children tree))))
    (loop for f in forms
          for i from 0
          do (format t "~&form ~A: ~S~%" i (kind f))
             (loop for c in (cl-tree-sitter:node-children f)
                   for j from 0
                   do (format t "   child ~A: ~S  text=~S~%"
                              j (kind c)
                              (ignore-errors
                               (clef-parser/parser:node-text c *source*)))))))
