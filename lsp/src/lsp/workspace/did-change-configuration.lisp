(in-package :clef-lsp/workspace)

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration
(defun handle-workspace-did-change-configuration (message)
       ;; Accepted and ignored: the server has no configurable settings yet. The
       ;; notification is still worth handling so clients that send it do not see
       ;; an unhandled-method response. Read the settings out of
       ;; (clef-jsonrpc/types:request-params message) when there is something to
       ;; do with them.
       (declare (ignore message))
       nil)
