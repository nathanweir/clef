(defpackage :clef-lsp/src/lsp/misc/exit
  (:use :cl)
  (:import-from :clef-lsp/src/log #:slog)
  (:local-nicknames
    (:server :clef-lsp/src/lsp/server))
  (:export
   #:handle-exit))

(in-package :clef-lsp/src/lsp/misc/exit)

;;;; The LSP `exit' notification.
;;;;
;;;; "Asks the server to exit its process." Not advisory -- it is the only way
;;;; a client has to end the server, and a server that ignores it outlives
;;;; every session that started one.
;;;;
;;;; This used to say "For now do nothing", and did exactly that: reset the
;;;; context and return. Combined with a read loop that had no exit condition,
;;;; nothing could ever stop the process -- not `exit', not stdin closing.
;;;; Driving the built binary over real stdio showed it surviving every one of
;;;; six termination scenarios, which the in-process test suite could not see
;;;; because it never runs the server as a process at all.

(defun handle-exit (message)
        (declare (ignore message))
        (slog :info "Received exit notification")
        (server:exit-server))
