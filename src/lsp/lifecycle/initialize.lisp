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

(deftype test-document-uri ()
    "https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri"
    'string)

(defschema test-workspace-folder
           (object "test-workspace-folder"
                   ((uri test-document-uri
                         :accessor workspace-folder-uri
                         :documentation "The associated URI for this workspace folder.")
                    (name string
                          :accessor workspace-folder-name
                          :documentation "The name of the workspace folder. Used to refer to this workspace folder in the user interface."))))

(defun hash-table-to-alist (hash)
    (let (alist)
        (maphash (lambda (k v)
                     (push (cons k v) alist))
                 hash)
        (nreverse alist)))

(defun handle-initialize (request)
    (format t "schema is ~A~%" (find-schema 'test-workspace-folder))
    ;; (format t "parsed is: ~A~%" (com.inuoe.jzon:parse "{ \"uri\": 123, \"name\": \"folder-name\" }"))
    (let* ((data (com.inuoe.jzon:parse "{ \"uri\": \"abc\", \"name\": \"folder-name\" }"))
           (is-valid (validate-with-schema (find-schema 'test-workspace-folder) (hash-table-to-alist data)))
           (workspace-folder (unserialize-with-schema
                              (find-schema 'test-workspace-folder)
                              "{ \"uri\": 123, \"name\": \"folder-name\" }"
                              :json)))
        (slog :debug "is-valid: ~A" is-valid)
        (slog :debug "Loaded workspace-folder: ~A" workspace-folder))

    "hello world")
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
