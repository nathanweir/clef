(in-package :clef-lsp/server)

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
(defvar *server-capabilities-json*
        (dict "capabilities"
              ;; openClose MUST be present: per LSP 3.17, when it is omitted the
              ;; client must not send didOpen/didClose. Spec-strict clients
              ;; (opencode) then never open a document, so ctx:documents stays
              ;; empty and every handler returns nothing. Zed sends them anyway.
              (dict "textDocumentSync" (dict "openClose" t
                                             "change" 1
                                             "save" t)
                    "documentFormattingProvider" t
                    "diagnosticProvider" (dict "interFileDependencies" nil
                                               "workspaceDiagnostics" t)
                    "definitionProvider" t
                    "referencesProvider" t
                    "documentHighlightProvider" t
                    "documentSymbolProvider" t
                    "workspaceSymbolProvider" t
                    "signatureHelpProvider" (dict)
                    "completionProvider"
                    (dict "triggerCharacters" '("(")
                          "completionItem" (dict "labelDetailsSupport" t))
                    "hoverProvider" t)))
