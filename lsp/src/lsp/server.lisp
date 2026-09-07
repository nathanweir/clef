(defpackage :clef-lsp/src/lsp/server
  (:use :cl)
  (:import-from :serapeum)
  (:import-from :clef-lsp/src/log #:slog)
  (:local-nicknames
    (:ctx :clef-lsp/src/context)
    (:error-codes :clef-lsp/src/lsp/types/base/error-codes)
    (:log :clef-lsp/src/log)
    (:messages :clef-lsp/src/jsonrpc/messages)
    (:rpc :clef-lsp/src/jsonrpc/types)
    (:symbols :clef-lsp/src/symbols/init))
  (:export
   #:*exit-terminates-process*
   #:before-handle-request
   #:exit-server
   #:handle-lsp-request
   #:publish-diagnostics
   #:reset
   #:send-notification
   #:sethandler
   #:start))

(in-package :clef-lsp/src/lsp/server)

;;; LSP server loop and handler dispatch.
;;;
;;; All persistent state lives on the CLEF-LSP/SRC/CONTEXT:SERVER-CONTEXT struct held
;;; in CLEF-LSP/SRC/CONTEXT:*SERVER*. This file used to own several defparameters
;;; (*initialized*, *documents*, *workspace-root*, ...) that have been moved
;;; there; see src/context.lisp for the canonical definitions.

(defparameter *index-consulting-methods*
  '("textDocument/definition"
    "textDocument/references"
    "textDocument/implementation"
    "textDocument/documentHighlight"
    "textDocument/hover"
    "textDocument/prepareCallHierarchy"
    "callHierarchy/incomingCalls"
    "callHierarchy/outgoingCalls"
    "workspace/symbol"
    "textDocument/rename"
    "textDocument/prepareRename")
  "Methods whose answers come from the workspace index rather than from the
open document, and which therefore need the index to be current.

Not every method: documentSymbol, semanticTokens, foldingRange and the rest
answer from the client's own copy of the document, which is current by
definition. Refreshing for those would be work with nothing to show for it.")

(defun before-handle-request (request)
       "Hook to run before handling any request."
       (let ((endpoint-name (rpc:request-method request)))
            ;; Error if server not initialized, unless this is one of the
            ;; methods that must work outside an initialized session.
            ;;
            ;; `exit' and `shutdown' belong on that list as much as `initialize'
            ;; does. The spec says exit "asks the server to exit its process"
            ;; unconditionally, and it is a client's only way to stop a server
            ;; that never finished starting. Blocking it meant a client that
            ;; gave up between `initialize' and `initialized' could not clean up
            ;; after itself: driving the binary over stdio, `initialize' then
            ;; `exit' left the process to be reaped by EOF instead, exiting 0
            ;; where the spec calls for 1.
            (when (and (not (member endpoint-name '("initialize" "initialized"
                                                    "shutdown" "exit")
                                    :test #'string=))
                       (not ctx:initialized))
                  (slog :error "Server not initialized yet.")
                  (error 'error-codes:server-not-initialized-error))
            ;; Pick up edits made outside the protocol before answering from the
            ;; index. Centralised here rather than repeated in nine handlers, so
            ;; a tenth cannot forget. See CLEF-LSP/SRC/SYMBOLS/INIT:REFRESH-STALE-INDEX.
            (when (member endpoint-name *index-consulting-methods* :test #'string=)
                  (ignore-errors (symbols:refresh-stale-index)))))

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
                            (unless (rpc:notification-p request)
                                    (make-instance 'rpc:jsonrpc-response
                                                   :result result
                                                   :id id)))
                   (respond-error (code message &optional data)
                                  ;; Notifications get nothing back even when the
                                  ;; handler fails. Previously an unknown method
                                  ;; or an uninitialised server produced an error
                                  ;; response carrying a null id, which is itself
                                  ;; a protocol violation.
                                  (unless (rpc:notification-p request)
                                          (make-instance 'rpc:jsonrpc-error-response
                                                         :error (make-instance 'rpc:jsonrpc-error
                                                                               :code code
                                                                               :message message
                                                                               :data data)
                                                         :id id))))
                  (handler-case
                    (handler-bind
                      ((error (lambda (e)
                                (declare (ignore e))
                                (setf captured-backtrace (capture-backtrace)))))
                      (let* ((endpoint-name (rpc:request-method request))
                             (handler (gethash endpoint-name ctx:handlers)))
                            (if handler
                                (respond (funcall handler request))
                                (progn
                                  (slog :error "[~A] No handler found" endpoint-name)
                                  (error 'error-codes:method-not-found-error
                                         :endpoint endpoint-name)))))
                    (error-codes:lsp-error (e)
                                                   (respond-error (error-codes:lsp-error-code e)
                                                                  (error-codes:lsp-error-message e)
                                                                  (ignore-errors (error-codes:lsp-error-data e))))
                    (error (e)
                           (slog :error "[~A] Internal error: ~A"
                                 (rpc:request-method request) e)
                           (when captured-backtrace
                                 (slog :error "Backtrace:~%~A" captured-backtrace))
                           (respond-error rpc:+internal-error+
                                          (format nil "Internal server error: ~A" e)))))))

(defparameter *exit-terminates-process* t
              "Whether the `exit' notification really ends the OS process.

Bound to NIL by the test suite, which calls the handler in-process and would
otherwise take the test runner down with it.")

(defun exit-server ()
       "End the server, as the LSP `exit' notification requires.

The spec is specific about the code: 0 if a `shutdown' request came first,
1 otherwise -- a client that never asked for shutdown is being told its server
died unexpectedly.

Returns the code instead of exiting when *EXIT-TERMINATES-PROCESS* is NIL."
       (let ((code (if ctx:shutdown-received 0 1)))
            (slog :info "Exiting with code ~D (shutdown ~:[was not~;was~] received)"
                  code ctx:shutdown-received)
            (reset)
            (if *exit-terminates-process*
                (sb-ext:exit :code code :abort nil)
                code)))

(defun run-lsp-server-stdio (&key (input *standard-input*) (output *standard-output*))
       "Run LSP server over stdio, until the client goes away."
       (setf ctx:output-stream output)
       (loop
         (let ((request (messages:read-lsp-message input)))
              ;; NIL means the stream is finished -- EOF, or a header we cannot
              ;; make sense of, after which there is no way to find where the
              ;; next message starts. READ-LSP-MESSAGE's own docstring says it
              ;; "returns NIL on EOF or stream error to allow graceful
              ;; shutdown"; the graceful shutdown was designed and never wired
              ;; up. This LOOP had no exit condition at all, so on EOF it spun
              ;; on a dead stream forever, burning a core and leaving the
              ;; process alive after every editor session.
              (unless request
                      (slog :info "Input stream closed; server loop exiting.")
                      (return))
              (let* ((id (rpc:request-id request))
                     (response (handle-lsp-request id request)))
                    ;; NIL here now means "notification" for real --
                    ;; HANDLE-LSP-REQUEST decides that from the id, not
                    ;; from what the handler happened to return.
                    (when response
                          (messages:write-lsp-message response output))))))

(defun send-notification (method params)
       "Send an LSP notification (a message with no id that doesn't expect a response)."
       (let ((stream ctx:output-stream))
            (when stream
                  (let ((notification (serapeum:dict
                                        "jsonrpc" "2.0"
                                        "method" method
                                        "params" params)))
                       (messages:write-lsp-message notification stream)))))

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

(defun reset ()
       "Discard all server state by installing a fresh context."
       (ctx:reset-context)
       (slog :info "CLEF LSP server state has been reset."))

(defun start (&key (input *standard-input*) (output *standard-output*)
                   (log-mode :none) log-file-path
                   (register (error "START needs a :REGISTER function")))
       "Starts the CLEF LSP server.

        REGISTER is called once, before the loop, to fill the handler table;
        the entry point passes CLEF-LSP/SRC/LSP/HANDLERS:REGISTER-HANDLERS. The server
        does not name its handlers itself -- they depend on it, so it must not
        depend on them (see handlers.lisp).

        LOG-MODE defaults to :none -- normal use writes no log file at all. The
        from-source launchers opt into :file with a project-local path; the
        binary opts in only when CLEF_LOG_FILE is set."

       ;; Controls verbosity and whether to output logs to console or a file
       (log:init log-mode :file-path log-file-path)

       (slog :debug "Starting CLEF LSP server...")
       (slog :debug "Registering handlers...")
       ;; OUTPUT was captured above and is the only stream the protocol may use.
       ;; Rebind *standard-output* so a stray format t anywhere in a handler or a
       ;; dependency lands on stderr instead of corrupting the client's stream.
       (let ((*standard-output* *error-output*))
            (funcall register)
            (run-lsp-server-stdio :input input :output output))
       (slog :info "Shutting down CLEF LSP server."))
