(in-package :clef-lsp/document)

(defun handle-text-document-did-save (message)
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (clef-util:cleanup-path (href params "text-document" "uri"))))
             (slog :debug "[textDocument/didSave] Document saved: ~A" document-uri)
             ;; For .asd files, re-parse and reload the systems defined in that file
             (when (uiop:string-suffix-p document-uri ".asd")
                   (reload-asd-file document-uri))
             ;; Rebuild symbol map for the saved file
             (let ((document-text (gethash (format nil "file://~A" document-uri)
                                           clef-lsp/server:*documents*)))
                  (when document-text
                        (clef-symbols:build-file-symbol-map document-uri document-text)))))

(defun reload-asd-file (asd-path)
       "Re-parse an .asd file and reload any changed systems."
       (slog :debug "Reloading .asd file: ~A" asd-path)
       (let ((new-systems (clef-lsp/lifecycle::parse-asd-file asd-path)))
            (dolist (sys new-systems)
                    (let ((name (clef-symbols:system-info-name sys)))
                         ;; Update or add the system info
                         (setf (gethash name clef-lsp/lifecycle::*loaded-systems*) sys)
                         ;; Reload the system
                         (clef-lsp/lifecycle::load-system-with-info sys)))
            ;; Rebuild file mapping
            (clef-lsp/lifecycle::build-file-to-system-mapping)))
