(in-package :clef-lsp/workspace)

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration
(defun handle-workspace-did-change-configuration (message)
       (let* ((params-hash (clef-jsonrpc/types:request-params message))
              (settings (href params-hash "settings")))
             ;; (slog :debug "Configuration changed: ~A" settings)
             ;; For now do nothing with the settings
             nil))
