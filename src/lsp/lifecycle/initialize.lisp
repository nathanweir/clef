(in-package :clef-lsp/lifecycle)

(defun get-initialize-params (request)
    "Extracts the initialize params from the LSP initialize request into a initialize-params object"
    (let* ((params-hash (clef-jsonrpc/types:request-params request))
           (process-id (gethash "processId" params-hash))
           (root-uri (gethash "rootUri" params-hash))
           (capabilities (gethash "capabilities" params-hash)))
        ;; (slog :debug "Parsed params hash: ~A" (com.inuoe.jzon:stringify params-hash))
        ;; (slog :debug "Extracted initialize params: process-id=~A, root-uri=~A, capabilities=<omitted>"
        ;;       process-id root-uri)
        (make-instance 'clef-lsp/types/lifecycle:initialize-params
            :process-id process-id
            :root-uri root-uri
            :capabilities capabilities)))

(defun handle-initialize (request)
    (let* ((params (get-initialize-params request))
           (process-id (clef-lsp/types/lifecycle:initialize-params-process-id params)))
        (slog :info "Handling 'initialize' with process-id: ~A" process-id)
        (format nil "Process-id is: ~A" process-id)))

;; (slog :info "Handling 'initialize' with process-id: ~A" process-id)
;; (slog :debug "Request-method is : ~A" (clef-jsonrpc/types:request-method request))
;; "my cool value 123")
;; (format nil "Process-id is: ~A" process-id))

;; (let* ((params (get-initialize-params request))
;;        (process-id (clef-lsp/types/lifecycle:initialize-params-process-id params)))
;;     (slog :info "Handling 'initialize' with process-id: ~A" process-id)
;;     (format nil "Process-id is: ~A" process-id)))
