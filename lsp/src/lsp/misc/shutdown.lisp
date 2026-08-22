(in-package :clef-lsp/misc)

(defun handle-shutdown (message)
  "Handle the LSP `shutdown' request.

Two things the previous version got wrong.

The result must be null. Returning (dict \"result\" nil) produced
{\"result\": {\"result\": null}}, because HANDLE-LSP-REQUEST already wraps a
handler's return value as the response's `result'. Handlers return the result
itself, not a response envelope.

And the shutdown must be *recorded*. CTX:SHUTDOWN-RECEIVED exists on the server
context for exactly this and nothing ever set it -- so the `exit' notification
had no way to tell an orderly shutdown from a client that vanished, which is
what decides the process exit code. RESET-CONTEXT replaces the context
wholesale, so the flag is set after the reset rather than before."
  ;; The message is part of the handler protocol, not something shutdown needs.
  (declare (ignore message))
  (slog :info "Received request to shut down server")
  (clef-lsp/server:reset)
  (setf ctx:shutdown-received t)
  nil)
