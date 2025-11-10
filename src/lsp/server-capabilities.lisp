(in-package :clef-lsp/server)

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
(defvar *server-capabilities-json*
        (dict "capabilities"
              (dict "textDocumentSync" (dict "change" 1
                                             "save" t)
                    "documentFormattingProvider" t
                    "diagnosticProvider" (dict "interFileDependencies" nil
                                               "workspaceDiagnostics" t)
                    "definitionProvider" t
                    "completionProvider"
                    (dict "triggerCharacters" '("(")
                          "completionItem" (dict "labelDetailsSupport" t))
                    "hoverProvider" t)))
