(in-package :clef-lsp/document)

(defun handle-text-document-completion (message)
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (href params "text-document" "uri"))
              (line (href params "position" "line"))
              (character (href params "position" "character"))
              ;; Each completion is a dict completion item for the LSP spec
              (completions (make-array 0 :adjustable t :fill-pointer 0)))
             (slog :debug "Completion request for URI: ~A at line ~A, character ~A" document-uri line character)

             (multiple-value-bind (ref-name ref-scope)
                                  (clef-symbols:get-ref-for-doc-pos document-uri line character)
                                  ;; Collect symbol-ref names from current and parent scopes
                                  (unless (not ref-name)
                                          (let ((scope ref-scope))
                                               (loop
                                                 while scope
                                                 do (let* ((defs (clef-symbols:lexical-scope-symbol-definitions scope))
                                                           (new-completions (make-completions defs)))
                                                          ;; Append new completions to the end of 'completions'
                                                          (dotimes (i (length new-completions))
                                                                   (vector-push-extend (aref new-completions i) completions)))
                                                 (setf scope (clef-symbols:lexical-scope-parent-scope scope))))))

             (dict "isIncomplete" nil
                   "items" completions)))


(defun determine-symbol-kind (symbol-def)
       "Determine the LSP SymbolKind for SYMBOL-DEF."
       (let ((kind (clef-symbols:symbol-definition-kind symbol-def)))
            (cond
              ((eq kind :function) 3)  ;; Function
              ((eq kind :macro) 3)     ;; Function
              ((eq kind :variable) 6)  ;; Variable
              ((eq kind :class) 5)     ;; Class
              ((eq kind :package) 9)   ;; Namespace
              ((eq kind :constant) 14) ;; Constant
              ((eq kind :type) 7)      ;; Type Parameter
              (t 1))))                 ;; Text (default)

(defun make-completions (symbol-defs)
       "Create LSP completion items from SYMBOL-DEFS."
       (let ((items (mapcar (lambda (def)
                                    (dict "label" (clef-symbols:symbol-definition-symbol-name def)
                                          "kind" (determine-symbol-kind def)))
                            ;; "documentation" (format nil "Definition of ~A"
                            ;;                         (clef-symbols:symbol-definition-symbol-name def))))
                            symbol-defs)))
            (apply #'vector items)))
