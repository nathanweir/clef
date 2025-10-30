(in-package :clef-root)

(defun start-server ()
       (slog :info "Starting CLEF...")
       (clef-lsp/server:start))

;(defun hi ()
;(format t "asdfa"))

;; test 1234
