(in-package :clef-lsp/server)

;;; LSP server loop and handler dispatch.
;;;
;;; All persistent state lives on the CLEF-CONTEXT:SERVER-CONTEXT struct held
;;; in CLEF-CONTEXT:*SERVER*. This file used to own several defparameters
;;; (*initialized*, *documents*, *workspace-root*, ...) that have been moved
;;; there; see src/context.lisp for the canonical definitions.

(defun before-handle-request (request)
       "Hook to run before handling any request."
       (let ((endpoint-name (clef-jsonrpc/types:request-method request)))
            ;; Error if server not initialized, unless these are requests to the endpoints that handle initialization
            (when (and (not (string= endpoint-name "initialize"))
                       (not (string= endpoint-name "initialized"))
                       (not ctx:initialized))
                  (slog :error "Server not initialized yet.")
                  (error 'clef-lsp/types/base:server-not-initialized-error))))

(defun capture-backtrace ()
       "Capture current backtrace as a string."
       (with-output-to-string (s)
         (sb-debug:print-backtrace :stream s :count 20)))

(defun handle-lsp-request (id request)
       (let ((captured-backtrace nil))
            (flet ((respond (result)
                            ;; NIL result is not silence -- it serialises to
                            ;; "result": null, which is the correct answer to a
                            ;; request that found nothing.
                            (unless (clef-jsonrpc/types:notification-p request)
                                    (make-instance 'clef-jsonrpc/types:jsonrpc-response
                                                   :result result
                                                   :id id)))
                   (respond-error (code message &optional data)
                                  ;; Notifications get nothing back even when the
                                  ;; handler fails. Previously an unknown method
                                  ;; or an uninitialised server produced an error
                                  ;; response carrying a null id, which is itself
                                  ;; a protocol violation.
                                  (unless (clef-jsonrpc/types:notification-p request)
                                          (make-instance 'clef-jsonrpc/types:jsonrpc-error-response
                                                         :error (make-instance 'clef-jsonrpc/types:jsonrpc-error
                                                                               :code code
                                                                               :message message
                                                                               :data data)
                                                         :id id))))
                  (handler-case
                    (handler-bind
                      ((error (lambda (e)
                                (declare (ignore e))
                                (setf captured-backtrace (capture-backtrace)))))
                      (let* ((endpoint-name (clef-jsonrpc/types:request-method request))
                             (handler (gethash endpoint-name ctx:handlers)))
                            (if handler
                                (respond (funcall handler request))
                                (progn
                                  (slog :error "[~A] No handler found" endpoint-name)
                                  (error 'clef-lsp/types/base:method-not-found-error
                                         :endpoint endpoint-name)))))
                    (clef-lsp/types/base:lsp-error (e)
                                                   (respond-error (clef-lsp/types/base:lsp-error-code e)
                                                                  (clef-lsp/types/base:lsp-error-message e)
                                                                  (ignore-errors (clef-lsp/types/base:lsp-error-data e))))
                    (error (e)
                           (slog :error "[~A] Internal error: ~A"
                                 (clef-jsonrpc/types:request-method request) e)
                           (when captured-backtrace
                                 (slog :error "Backtrace:~%~A" captured-backtrace))
                           (respond-error clef-jsonrpc/types:+internal-error+
                                          (format nil "Internal server error: ~A" e)))))))

(defun run-lsp-server-stdio (&key (input *standard-input*) (output *standard-output*))
       "Run LSP server over stdio"
       (setf ctx:output-stream output)
       (loop
         (let ((request (clef-jsonrpc/messages:read-lsp-message input)))
              (when request
                    (let* ((id (clef-jsonrpc/types:request-id request))
                           (response (handle-lsp-request id request)))
                          ;; NIL here now means "notification" for real --
                          ;; HANDLE-LSP-REQUEST decides that from the id, not
                          ;; from what the handler happened to return.
                          (when response
                                (clef-jsonrpc/messages:write-lsp-message response output)))))))

(defun send-notification (method params)
       "Send an LSP notification (a message with no id that doesn't expect a response)."
       (let ((stream ctx:output-stream))
            (when stream
                  (let ((notification (serapeum:dict
                                        "jsonrpc" "2.0"
                                        "method" method
                                        "params" params)))
                       (clef-jsonrpc/messages:write-lsp-message notification stream)))))

(defun publish-diagnostics (uri diagnostics)
       "Publish diagnostics for a document using textDocument/publishDiagnostics notification."
       (send-notification "textDocument/publishDiagnostics"
                          (serapeum:dict "uri" uri
                                         "diagnostics" (or diagnostics #()))))

(defun sethandler (endpoint-name handler-lambda)
       "Defines an LSP handler for the given endpoint name."
       (slog :debug "Defining LSP handler for endpoint: ~A" endpoint-name)
       (setf (gethash endpoint-name ctx:handlers)
             (lambda (request)
                     (before-handle-request request)
                     (funcall handler-lambda request))))

(defun register-handlers ()
       "Registers all LSP handlers on the current context."
       (sethandler "initialize" 'clef-lsp/lifecycle:handle-initialize)
       (sethandler "initialized" 'clef-lsp/lifecycle:handle-initialized)
       (sethandler "textDocument/completion" 'clef-lsp/document:handle-text-document-completion)
       (sethandler "textDocument/definition" 'clef-lsp/document:handle-text-document-definition)
       (sethandler "textDocument/references" 'clef-lsp/document:handle-text-document-references)
       (sethandler "textDocument/didOpen" 'clef-lsp/document:handle-text-document-did-open)
       (sethandler "textDocument/didChange" 'clef-lsp/document:handle-text-document-did-change)
       (sethandler "textDocument/didClose" 'clef-lsp/document:handle-text-document-did-close)
       (sethandler "textDocument/didSave" 'clef-lsp/document:handle-text-document-did-save)
       (sethandler "textDocument/formatting" 'clef-lsp/document:handle-text-document-formatting)
       (sethandler "textDocument/diagnostic" 'clef-lsp/document:handle-text-document-diagnostic)
       (sethandler "textDocument/hover" 'clef-lsp/document:handle-text-document-hover)
       (sethandler "textDocument/documentHighlight" 'clef-lsp/document:handle-text-document-highlight)
       (sethandler "textDocument/documentSymbol" 'clef-lsp/document:handle-text-document-document-symbol)
       (sethandler "textDocument/prepareCallHierarchy" 'clef-lsp/document:handle-text-document-prepare-call-hierarchy)
       (sethandler "callHierarchy/incomingCalls" 'clef-lsp/document:handle-call-hierarchy-incoming-calls)
       (sethandler "callHierarchy/outgoingCalls" 'clef-lsp/document:handle-call-hierarchy-outgoing-calls)
       (sethandler "textDocument/implementation" 'clef-lsp/document:handle-text-document-implementation)
       (sethandler "textDocument/signatureHelp" 'clef-lsp/document:handle-text-document-signature-help)
       (sethandler "workspace/diagnostic" 'clef-lsp/workspace:handle-workspace-diagnostic)
       (sethandler "workspace/didChangeConfiguration" 'clef-lsp/workspace:handle-workspace-did-change-configuration)
       (sethandler "workspace/symbol" 'clef-lsp/workspace:handle-workspace-symbol)
       (sethandler "shutdown" 'clef-lsp/misc:handle-shutdown)
       (sethandler "exit" 'clef-lsp/misc:handle-exit))

(defun reset ()
       "Discard all server state by installing a fresh context."
       (ctx:reset-context)
       (slog :info "CLEF LSP server state has been reset."))

(defun start (&key (input *standard-input*) (output *standard-output*)
                   (log-mode :none) log-file-path)
       "Starts the CLEF LSP server.

        LOG-MODE defaults to :none -- normal use writes no log file at all. The
        from-source launchers opt into :file with a project-local path; the
        binary opts in only when CLEF_LOG_FILE is set."

       ;; Controls verbosity and whether to output logs to console or a file
       (clef-log:init log-mode :file-path log-file-path)

       (slog :debug "Starting CLEF LSP server...")
       (slog :debug "Registering handlers...")
       ;; OUTPUT was captured above and is the only stream the protocol may use.
       ;; Rebind *standard-output* so a stray format t anywhere in a handler or a
       ;; dependency lands on stderr instead of corrupting the client's stream.
       (let ((*standard-output* *error-output*))
            (register-handlers)
            (run-lsp-server-stdio :input input :output output))
       (slog :info "Shutting down CLEF LSP server."))
