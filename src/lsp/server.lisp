(in-package :clef-lsp/server)

(defparameter *initialized* nil
              "Whether the client has responded with initialized. The server only responds
with ServerNotInitialized = -32002 before this occurs.")

(defparameter *handlers* (make-hash-table :test 'equal)
              "A hash table mapping LSP endpoint names to their handler functions.")

(defparameter *client-capabilities* nil
              "Stores a hash table of the client's capabilities as reported during initialization.
Seehttps://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#clientCapabilities ")

(defvar *documents* (make-hash-table :test 'equal)
        "Document objects kept in memory and maintained by textDocument/didOpen, didChange, didClose requests.
Top level keys are file paths/URIs, values are the full text of the documents as strings. Newline chars. are preserved with
(currently) no consideration of cross-platform differences.")

(defun before-handle-request (request)
       "Hook to run before handling any request."
       (let ((endpoint-name (clef-jsonrpc/types:request-method request)))
            ;; Error if server not initialized, unless these are requests to the endpoints that handle initialization
            (when (and (not (string= endpoint-name "initialize"))
                       (not (string= endpoint-name "initialized"))
                       (not *initialized*))
                  (slog :error "Server not initialized yet.")
                  (error 'clef-lsp/types/base:server-not-initialized-error))))

(defun handle-lsp-request (id request)
       ;; (slog :info "Received LSP request: ~A" request)
       (handler-case
         ;; TODO: Actually create an appropriate response
         (let* ((endpoint-name (clef-jsonrpc/types:request-method request)))
               ;; Check if the handler exists
               (let ((handler (gethash endpoint-name *handlers*)))
                    (if handler
                        (let ((message (funcall handler request)))
                             (slog :debug "[~A] ✓" endpoint-name)
                             ;; If message is 'nil', then we should return no response. Return nil here
                             ;; TODO: This is my hacky support for notifications, but could probably define this
                             ;; as a param or elsewhere
                             (if (null message)
                                 nil
                                 (make-instance 'clef-jsonrpc/types:jsonrpc-response
                                                :result message
                                                :id id)))
                        (progn
                          (slog :error "[~A] No handler found" endpoint-name)
                          (error 'clef-lsp/types/base:method-not-found-error :endpoint endpoint-name)))))
         (clef-lsp/types/base:lsp-error (e)
                                        ;; (slog :error "[LSP error handling request: ~A" e)
                                        (make-instance 'clef-jsonrpc/types:jsonrpc-error-response
                                                       :error (make-instance 'clef-jsonrpc/types:jsonrpc-error
                                                                             :code (clef-lsp/types/base:lsp-error-code e)
                                                                             :message (clef-lsp/types/base:lsp-error-message e)
                                                                             :data (ignore-errors (clef-lsp/types/base:lsp-error-data e)))
                                                       :id id))
         (error (e)
                ;; TODO: Find a way to capture a backtrace here
                (slog :error "[~A] Internal error handling request: ~A" (clef-jsonrpc/types:request-method request) e)
                (make-instance 'clef-jsonrpc/types:jsonrpc-error-response
                               :error (make-instance 'clef-jsonrpc/types:jsonrpc-error
                                                     :code clef-jsonrpc/types:+internal-error+
                                                     :message (format nil "Internal server error: ~A" e))
                               :id id))))

(defun run-lsp-server-stdio (&key (input *standard-input*) (output *standard-output*))
       "Run LSP server over stdio"
       (loop
         (let ((request (clef-jsonrpc/messages:read-lsp-message input)))
              (when request
                    (let* ((id (clef-jsonrpc/types:request-id request))
                           (response (handle-lsp-request id request)))
                          ;; Skip sending the response if 'response' is nil, meaning this was a notification
                          (when response
                                (clef-jsonrpc/messages:write-lsp-message response output)))))))

(defun sethandler (endpoint-name handler-lambda)
       "Defines an LSP handler for the given endpoint name."
       (slog :debug "Defining LSP handler for endpoint: ~A" endpoint-name)
       (setf (gethash endpoint-name *handlers*)
             (lambda (request)
                     ;; From src/lsp/server.lisp
                     (before-handle-request request)
                     (slog :debug "[~A] →" endpoint-name)
                     (funcall handler-lambda request))))

;; TODO: It'd be cool to make a macro for registering handlers and not requiring exporting them + doing
;; setup there... but probably no real point.
(defun register-handlers ()
       "Registers all LSP handlers from *handlers*"
       ;; For now, just a bunch of manual sethandler calls. Need to reconsider this later
       (sethandler "initialize" 'clef-lsp/lifecycle:handle-initialize)
       (sethandler "initialized" 'clef-lsp/lifecycle:handle-initialized)
       (sethandler "textDocument/didOpen" 'clef-lsp/document:handle-text-document-did-open)
       (sethandler "textDocument/didChange" 'clef-lsp/document:handle-text-document-did-change)
       (sethandler "textDocument/formatting" 'clef-lsp/document:handle-text-document-formatting)
       (sethandler "textDocument/diagnostic" 'clef-lsp/document:handle-text-document-diagnostic)
       (sethandler "textDocument/hover" 'clef-lsp/document:handle-text-document-hover)
       (sethandler "workspace/diagnostic" 'clef-lsp/workspace:handle-workspace-diagnostic)
       (sethandler "workspace/didChangeConfiguration" 'clef-lsp/workspace:handle-workspace-did-change-configuration)
       (sethandler "shutdown" 'clef-lsp/misc:handle-shutdown)
       (sethandler "exit" 'clef-lsp/misc:handle-exit))

(defun reset ()
       "Resets all server state, to be called when the server is asked to shutdown or exit"
       (setf *initialized* nil)
       (setf *client-capabilities* nil)
       (setf *documents* (make-hash-table :test 'equal))
       (slog :info "CLEF LSP server state has been reset."))

(defun start (&key (input *standard-input*) (output *standard-output*) (log-mode :file))
       "Starts the CLEF LSP server."

       ;; Controls verbosity and whether to output logs to console or a file
       (clef-log:init log-mode)

       ;; TODO: Spawn the server in a new thread, watch for crashes, and restart if that occurs.
       ;; Also listen for & handle LSP messages to shut down / restart the server
       (slog :debug "Starting CLEF LSP server...")
       (slog :debug "Registering handlers...")
       (register-handlers)
       (run-lsp-server-stdio :input input :output output)
       (slog :info "Shutting down CLEF LSP server."))
