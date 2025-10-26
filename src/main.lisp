(in-package :clef-root)

(defun start-server ()
    (slog :info "Starting CLEF...")
    (clef-lsp/server:start))
