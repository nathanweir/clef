(in-package :clef-lsp/server)

(defparameter *handlers* (make-hash-table :test 'equal)
              "A hash table mapping LSP endpoint names to their handler functions.")

(defparameter *initialized* nil
              "Whether the client has responded with initialized. The server only responds
with ServerNotInitialized = -32002 before this occurs.")

(defun before-handle-request (request)
    "Hook to run before handling any request."
    (slog :debug "*handlers* is: ~A" *handlers*)
    (let ((endpoint-name (clef-jsonrpc/types:request-method request)))
        ;; Error if server not initialized, unless these are requests to the endpoints that handle initialization
        (when (and (not (string= endpoint-name "initialize"))
                   (not (string= endpoint-name "initialized"))
                   (not *initialized*))
              (slog :error "Server not initialized yet.")
              (error 'clef-lsp/types/base:server-not-initialized-error))
        ;; Otherwise, handle the request
        (let ((handler (gethash endpoint-name *handlers*)))
            (if handler
                (funcall handler request)
                (progn
                 (slog :error "No handler found for endpoint: ~A" endpoint-name)
                 (error 'clef-lsp/types/base:method-not-found-error :endpoint endpoint-name))))))

(defun sethandler (endpoint-name handler-lambda)
    "Defines an LSP handler for the given endpoint name."
    (slog :debug "Defining LSP handler for endpoint: ~A" endpoint-name)
    (setf (gethash endpoint-name *handlers*)
        (lambda (request)
            ;; From src/lsp/server.lisp
            (before-handle-request request)
            (funcall handler-lambda request))))

(defun handle-lsp-request (id request)
    (slog :info "Received LSP request: ~A" request)
    (handler-case
            (progn
             ;; (before-handle-request request)
             (let ((message "pickle 123"))
                 (make-instance 'clef-jsonrpc/types:jsonrpc-response
                     :result message
                     :id id)))
        (clef-lsp/types/base:lsp-error (e)
                                       (slog :error "LSP error handling request: ~A" e)
                                       (make-instance 'clef-jsonrpc/types:jsonrpc-error-response
                                           :error (make-instance 'clef-jsonrpc/types:jsonrpc-error
                                                      :code (clef-lsp/types/base:lsp-error-code e)
                                                      :message (clef-lsp/types/base:lsp-error-message e)
                                                      :data (ignore-errors (clef-lsp/types/base:lsp-error-data e)))
                                           :id id))
        (error (e)
            ;; TODO: Find a way to capture a backtrace here
            (slog :error "Internal error handling request: ~A" e)
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
                   (clef-jsonrpc/messages:write-lsp-message response output))))))

;; TODO: It'd be cool to make a macro for registering handlers and not requiring exporting them + doing
;; setup there... but probably no real point.
(defun register-handlers ()
    "Registers all LSP handlers."
    (sethandler "initialize" 'clef-lsp/lifecycle:handle-initialize))

(defun start (&key (input *standard-input*) (output *standard-output*) (log-mode :file))
    "Starts the CLEF LSP server."

    ;; Controls verbosity and whether to output logs to console or a file
    (clef-log:init log-mode)

    ;; TODO: Spawn the server in a new thread, watch for crashes, and restart if that occurs.
    ;; Also listen for & handle LSP messages to shut down / restart the server
    (slog :debug "Starting CLEF LSP server...")

    (slog "Handlers are: ~A~%" *handlers*)
    (run-lsp-server-stdio :input input :output output)
    (slog :info "Shutting down CLEF LSP server."))
