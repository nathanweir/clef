(defpackage :clef-lsp/src/lsp/document/did-save
  (:use :cl)
  (:import-from :clef-lsp/src/log #:slog)
  (:import-from :serapeum #:href)
  (:local-nicknames
    (:ctx :clef-lsp/src/context)
    (:initialize :clef-lsp/src/lsp/lifecycle/initialize)
    (:rpc :clef-lsp/src/jsonrpc/types)
    (:sym :clef-lsp/src/symbols/types)
    (:symbols :clef-lsp/src/symbols/init)
    (:util :clef-lsp/src/util))
  (:export
   #:handle-text-document-did-save))

(in-package :clef-lsp/src/lsp/document/did-save)

(defun handle-text-document-did-save (message)
       (let* ((params (rpc:request-params message))
              (document-uri (util:cleanup-path (href params "text-document" "uri"))))
             (slog :debug "[textDocument/didSave] Document saved: ~A" document-uri)
             ;; For .asd files, re-parse and reload the systems defined in that file
             (when (uiop:string-suffix-p document-uri ".asd")
                   (reload-asd-file document-uri))
             ;; Rebuild symbol map for the saved file
             (let ((document-text (gethash (format nil "file://~A" document-uri)
                                           ctx:documents)))
                  (when document-text
                        (symbols:build-file-symbol-map document-uri document-text)))))

(defun reload-asd-file (asd-path)
       "Re-parse an .asd file and reload any changed systems."
       (slog :debug "Reloading .asd file: ~A" asd-path)
       (let ((new-systems (initialize:parse-asd-file asd-path)))
            (dolist (sys new-systems)
                    (let ((name (sym:system-info-name sys)))
                         ;; Update or add the system info
                         (setf (gethash name ctx:loaded-systems) sys)
                         ;; Reload the system
                         (initialize:load-system-with-info sys)))
            ;; Rebuild file mapping
            (initialize:build-file-to-system-mapping)))
