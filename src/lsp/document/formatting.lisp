(in-package :clef-lsp/document)

(indentify:load-default-templates)

;; TODO: Really not sure if indentify is OK to use long-term, but we definitely need to honor the various
;; formatting options: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#formattingOptions
(defun handle-text-document-formatting (message)
    (let* ((params (clef-jsonrpc/types:request-params message))
           (file-uri (href params "text-document" "uri"))
           (pos (href params "position"))
           (options (href params "options")))
        ;; Format the entire file with cl-indentify
        (slog :debug "About to do formatting on ~A" file-uri)
        (let ((output (make-string-output-stream)))
            (indentify:indentify
             (make-string-input-stream (gethash file-uri clef-lsp/server:*documents*))
             output)
            (slog :debug "Did formatting on ~A" file-uri)
            (slog :debug "output: ~A" (get-output-stream-string output))
            (setf (gethash file-uri clef-lsp/server:*documents*)
                (get-output-stream-string output)))
        ;; (slog :debug "~A" )
        ;; (let ((output (make-string-output-stream)))
        ;;     (indentify:indentify
        ;;      (make-string-input-stream (gethash file-uri clef-lsp*documents*))
        ;;      output)
        ;;     (setf (gethash file-uri clef-lsp*documents*) (get-output-stream-string output))))
        ;; (edits (format-lisp-pos (gethash file-uri clef-lsp*documents*) pos options)))
        nil))
