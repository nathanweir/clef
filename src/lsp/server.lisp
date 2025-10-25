(in-package :clef-lsp/server)

(defparameter *initialized* nil
              "Whether the client has responded with initialized. The server only responds
with ServerNotInitialized = -32002 before this occurs.")

;; (defun before-handle-request (endpoint-name args)
;;     (declare (ignore args))
;;     "Hook to run before handling any request."
;;     ;; Error if server not initialized, unless these are requests to the endpoints that handle initialization
;;     (when (and (not (string= endpoint-name "initialize"))
;;                (not (string= endpoint-name "initialized"))
;;                (not *initialized*))
;;           (format t "Server not initialized yet.~%")
;;           ;; TODO: Seemingly this does in fact serface the correct kind of error response to the client, even
;;           ;; though I've done no managing of the result vs error prop in ResponseMessage
;;           ;; (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage).
;;           ;; I'm unsure whether that's handled at the JSONRPC level or if I do still need to implement that type and
;;           ;; manually construct.
;;           (error 'jsonrpc:jsonrpc-error
;;               :code clef-lsp/types/base:+server-not-initialized+
;;               :message "The LSP has not yet been initialized."
;;               :data "The client must call the 'initialized' endpoint before sending other messages")))

(defun handle-lsp-request (request)
    (format t "Received LSP request: ~A~%" request))

(defun run-lsp-server-stdio (&key (input *standard-input*) (output *standard-output*))
    "Run LSP server over stdio"
    (loop
     (let ((request (clef-jsonrpc/messages:read-lsp-message input)))
         (when request
               (let* ((id (clef-jsonrpc/types:request-id request))
                      (response (handle-lsp-request request)))
                   (clef-jsonrpc/messages:write-lsp-message id "test 123" output))))))

(defun start (&key (input *standard-input*) (output *standard-output*))
    "Starts the CLEF LSP server."
    ;; TODO: Spawn the server in a new thread, watch for crashes, and restart if that occurs.
    ;; Also listen for & handle LSP messages to shut down / restart the server
    (format t "Starting CLEF LSP server...~%")
    (run-lsp-server-stdio :input input :output output)
    (format t "Shutting down CLEF LSP server.~%"))
