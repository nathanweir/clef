(in-package :clef-lsp/server)

(defparameter *lsp-host* "127.0.0.1")
;; TODO: Does this need to be randomized & stored, once per LSP session?
;; From https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageServerProtocol,
;; "The protocol currently assumes that one server serves one tool. There is currently no support in the protocol to share
;; one server between different tools. Such sharing would require additional protocol e.g. to lock a document to support
;; concurrent editing."
(defparameter *lsp-port* 8666) ;; Do NOT use 8006, as that's used by Alive LSP
;; The server gets made on program start and not (start) below as it must exist
;; before all the various server expose (via defhandler) calls are made
(defparameter *server* (jsonrpc:make-server))
(defparameter *initialized* nil
              "Whether the client has responded with initialized. The server only responds
with ServerNotInitialized = -32002 before this occurs.")

(defun before-handle-request (args)
    "Hook to run before handling any request."
    (when (not *initialized*)
          (format t "Server not initialized yet.~%")
          ;; TODO: Seemingly this does in fact serface the correct kind of error response to the client, even
          ;; though I've done no managing of the result vs error prop in ResponseMessage
          ;; (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage).
          ;; I'm unsure whether that's handled at the JSONRPC level or if I do still need to implement that type and
          ;; manually construct.
          (error 'jsonrpc:jsonrpc-error
              :code clef-lsp/types/base:+server-not-initialized+
              :message "The LSP has not yet been initialized."
              :data "The client must call the 'initialized' endpoint before sending other messages")))

(defun start ()
    "Starts the CLEF LSP server."
    (format t "Starting CLEF LSP server on ~A:~A...~%" *lsp-host* *lsp-port*)
    (jsonrpc:server-listen *server* :port *lsp-port* :mode :tcp)
    (format t "Shutting down CLEF LSP server.~%"))
;; (format t "CLEF LSP server is now listening on ~A:~A.~%" *lsp-host* *lsp-port*)
;; Placeholder for server initialization logic

;; The below is not necessary as server-listen does itself take over the
;; main thread.

;; Create a position just to verify it's possible
;;    (let ((pos (make-instance 'clef-lsp/types/basic:position :line 10 :character 5)))
;;        (format t "Created position: line=~A, character=~A~%"
;;            (clef-lsp/types/basic:position-line pos)
;;            (clef-lsp/types/basic:position-character pos)))
;;    (format t "CLEF LSP server started successfully.~%")
;;    (format t "Press ESC > Enter to stop the server...~%")
;;    (loop
;; for ch = (read-char)
;; until (char= ch #\Esc)
;; do (sleep 0.1))
;;    (format t "ESC pressed. Shutting down server.~%"))
