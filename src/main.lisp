(in-package :clef-root)

(defun start-server ()
    (format t "Starting CLEF...~%")
    (clef-lsp/server:start))
