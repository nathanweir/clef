(in-package :clef-lsp/misc)

;; TODO: Ths is effectively the same as 'shutdown' (other than responding with no message,
;; correctly as a notification handler). Long-term this should probably kill and restart
;; a language server process?
(defun handle-exit (message)
        (slog :info "Received request to exit down server")
        (clef-lsp/server:reset)
       ;; For now do nothing
       nil)
