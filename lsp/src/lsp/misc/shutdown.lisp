(in-package :clef-lsp/misc)

(defun handle-shutdown (message)
  ;; The message is part of the handler protocol, not something shutdown needs.
  (declare (ignore message))
  (slog :info "Received request to shut down server")
  (clef-lsp/server:reset)
  ;; Expected to return result: nil if no errors
  (dict "result" nil))
