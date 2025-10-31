(in-package :clef-lsp/document)

(defun get-last-line-info (file-content)
       "Returns (line-number . last-char-position-on-line)
   line-number: the line number of the last line (1-indexed)
   last-char-position-on-line: the position of the last character on that line (1-indexed)"
       (let* ((length (length file-content))
              (last-newline-pos (position #\newline file-content :from-end t)))
             (let ((line-number (+ 1 (count #\newline file-content)))
                   (last-char-pos (if last-newline-pos
                                      (- length last-newline-pos 1)
                                      length)))
                  (values line-number last-char-pos))))

;; TODO: Really not sure if indentify is OK to use long-term, but we definitely need to honor the various
;; formatting options: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#formattingOptions
(defun handle-text-document-formatting (message)
       (let* ((params (clef-jsonrpc/types:request-params message))
              (file-uri (href params "text-document" "uri"))
              (document-text (gethash file-uri clef-lsp/server:*documents*))
              (pos (href params "position"))
              (options (href params "options")))
             ;; Format the entire file with cl-indentify
             (slog :debug "About to do formatting on ~A" file-uri)
             (let ((output (make-string-output-stream)))
                  (indentify:indentify
                    (make-string-input-stream document-text)
                    output)
                  (let* ((output-string (get-output-stream-string output)))
                        (multiple-value-bind (last-line last-char) (get-last-line-info document-text)
                                             (slog :debug "Did formatting on ~A" file-uri)
                                             (slog :debug "lastline: ~A last-char: ~A" last-line last-char)
                                        (let ((edits (list (dict "range" (dict "start" (dict "line" 0 "character" 0)
                                                                               "end" (dict "line" last-line "character" last-char))
                                                                 "newText" output-string))))
                                                ;; (list)))))))
                                                edits))))))
