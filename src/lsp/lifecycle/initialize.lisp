(in-package :clef-lsp/lifecycle)

;; (defun get-initialize-params (request)
;;     "Extracts the initialize params from the LSP initialize request into a initialize-params object"
;;     (let* ((params-hash (clef-jsonrpc/types:request-params request))
;;            (process-id (gethash "processId" params-hash))
;;            (root-uri (gethash "rootUri" params-hash))
;;            (capabilities (gethash "capabilities" params-hash)))
;;         ;; (slog :debug "Parsed params hash: ~A" (com.inuoe.jzon:stringify params-hash))
;;         ;; (slog :debug "Extracted initialize params: process-id=~A, root-uri=~A, capabilities=<omitted>"
;;         ;;       process-id root-uri)
;;         (make-instance 'clef-lsp/types/lifecycle:initialize-params
;;             :process-id process-id
;;             :root-uri root-uri
;;             :capabilities capabilities)))

;; AI-slop; I haven't even read this
;; (defun hash-table-to-alist (hash)
;;     (let (alist)
;;         (maphash (lambda (k v)
;;                      (push (cons k
;;                                  (cond
;;                                   ((hash-table-p v) (hash-table-to-alist v))
;;                                   ((and (listp v) (not (stringp v)))
;;                                       (mapcar (lambda (item)
;;                                                   (if (hash-table-p item)
;;                                                       (hash-table-to-alist item)
;;                                                       item))
;;                                               v))
;;                                   ((and (vectorp v) (not (stringp v)))
;;                                       (map 'vector (lambda (item)
;;                                                        (if (hash-table-p item)
;;                                                            (hash-table-to-alist item)
;;                                                            item))
;;                                           v))
;;                                   (t v)))
;;                            alist))
;;                  hash)
;;         (nreverse alist)))

(defun handle-initialize (request)

    (let* ((params-hash (clef-jsonrpc/types:request-params request))
           (capabilities (gethash "capabilities" params-hash)))
        (setf clef-lsp/server:*client-capabilities* capabilities)

        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult
        (dict "capabilities"
              (dict "textDocumentSync" (dict "change" 1)
                    "documentFormattingProvider" t))))
;; (slog :debug "client-info: ~A~%" (serapeum:pretty-print-hash-table (gethash "client-info" params-hash)))
;; (slog :debug "type of client-info.version: ~A ~A"
;;       (type-of (gethash "client-info" params-hash))
;;       (type-of (gethash "version" (gethash "client-info" params-hash))))

;; This automatically throws an error (and seemingly returns nothing, so errors must be)
;; propagated this way)
;; (validate-with-schema
;;  (find-schema 'clef-lsp/types/lifecycle:initialize-params)
;;  (hash-table-to-alist params-hash))

;; "hello world"))

;; (format t "schema is ~A~%" (find-schema 'test-workspace-folder))
;; ;; (format t "parsed is: ~A~%" (com.inuoe.jzon:parse "{ \"uri\": 123, \"name\": \"folder-name\" }"))
;; (let* ((data (com.inuoe.jzon:parse "{ \"uri\": \"abc\", \"name\": \"folder-name\" }"))
;;        (is-valid (validate-with-schema (find-schema 'test-workspace-folder) (hash-table-to-alist data)))
;;        (workspace-folder (unserialize-with-schema
;;                           (find-schema 'test-workspace-folder)
;;                           "{ \"uri\": 123, \"name\": \"folder-name\" }"
;;                           :json)))
;;     (slog :debug "is-valid: ~A" is-valid)
;;     (slog :debug "Loaded workspace-folder: ~A" workspace-folder))

;; "hello world")
;; (let* ((params (get-initialize-params request))
;;        (process-id (clef-lsp/types/lifecycle:initialize-params-process-id params)))
;;     (slog :info "Handling 'initialize' with process-id: ~A" process-id)
;;     (format nil "Process-id is: ~A" process-id)))

;; (slog :info "Handling 'initialize' with process-id: ~A" process-id)
;; (slog :debug "Request-method is : ~A" (clef-jsonrpc/types:request-method request))
;; "my cool value 123")
;; (format nil "Process-id is: ~A" process-id))

;; (let* ((params (get-initialize-params request))
;;        (process-id (clef-lsp/types/lifecycle:initialize-params-process-id params)))
;;     (slog :info "Handling 'initialize' with process-id: ~A" process-id)
;;     (format nil "Process-id is: ~A" process-id)))
