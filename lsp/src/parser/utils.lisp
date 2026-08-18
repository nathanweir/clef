(in-package :clef-parser/utils)

(defun find-package-declaration (tree source)
       "Find (in-package ...) form in the tree-sitter AST."
       (labels ((visit-node (node)
                            (when node
                                  (let ((text (clef-parser/parser:node-text node source)))
                                       ;; Look for (in-package ...) forms
                                       (when (and (eq (ts:node-type node) :LIST-LIT)
                                                  (search "in-package" text))
                                             (handler-case
                                               (let ((form (read-from-string text)))
                                                    (when (and (consp form)
                                                               (eq (car form) 'in-package))
                                                          (return-from find-package-declaration
                                                                       (find-package (second form)))))
                                               (error () nil))))

                                  ;; Visit children
                                  (dolist (child (ts:node-children node))
                                          (when child (visit-node child))))))
               (when tree (visit-node tree))
               nil)) ; Return nil if no package found
