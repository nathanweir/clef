(in-package :clef-lsp/handlers)

;;;; The handler table: which LSP method is served by which function.
;;;;
;;;; This used to live in server.lisp, which made the server depend on every
;;;; handler module while the shutdown and exit handlers depended on the
;;;; server (RESET, EXIT-SERVER) -- a cycle that the hand-ordered .asd papered
;;;; over and that package inference refuses outright. The table now sits
;;;; above both: it imports the server for SETHANDLER and every handler
;;;; module for its functions, and nothing imports it except the entry point,
;;;; which hands it to START.

(defun register-handlers ()
  "Registers all LSP handlers on the current context."
  (flet ((sethandler (name fn) (clef-lsp/server:sethandler name fn)))
    (sethandler "initialize" 'clef-lsp/lifecycle:handle-initialize)
    (sethandler "initialized" 'clef-lsp/lifecycle:handle-initialized)
    (sethandler "textDocument/completion" 'clef-lsp/document:handle-text-document-completion)
    (sethandler "textDocument/definition" 'clef-lsp/document:handle-text-document-definition)
    (sethandler "textDocument/references" 'clef-lsp/document:handle-text-document-references)
    (sethandler "textDocument/didOpen" 'clef-lsp/document:handle-text-document-did-open)
    (sethandler "textDocument/didChange" 'clef-lsp/document:handle-text-document-did-change)
    (sethandler "textDocument/didClose" 'clef-lsp/document:handle-text-document-did-close)
    (sethandler "textDocument/didSave" 'clef-lsp/document:handle-text-document-did-save)
    (sethandler "textDocument/formatting" 'clef-lsp/document:handle-text-document-formatting)
    (sethandler "textDocument/diagnostic" 'clef-lsp/document:handle-text-document-diagnostic)
    (sethandler "textDocument/hover" 'clef-lsp/document:handle-text-document-hover)
    (sethandler "textDocument/documentHighlight" 'clef-lsp/document:handle-text-document-highlight)
    (sethandler "textDocument/documentSymbol" 'clef-lsp/document:handle-text-document-document-symbol)
    (sethandler "textDocument/prepareCallHierarchy" 'clef-lsp/document:handle-text-document-prepare-call-hierarchy)
    (sethandler "callHierarchy/incomingCalls" 'clef-lsp/document:handle-call-hierarchy-incoming-calls)
    (sethandler "callHierarchy/outgoingCalls" 'clef-lsp/document:handle-call-hierarchy-outgoing-calls)
    (sethandler "textDocument/implementation" 'clef-lsp/document:handle-text-document-implementation)
    (sethandler "textDocument/foldingRange" 'clef-lsp/document:handle-text-document-folding-range)
    (sethandler "textDocument/selectionRange" 'clef-lsp/document:handle-text-document-selection-range)
    (sethandler "textDocument/semanticTokens/full" 'clef-lsp/document:handle-text-document-semantic-tokens-full)
    (sethandler "textDocument/inlayHint" 'clef-lsp/document:handle-text-document-inlay-hint)
    (sethandler "textDocument/codeLens" 'clef-lsp/document:handle-text-document-code-lens)
    (sethandler "textDocument/rename" 'clef-lsp/document:handle-text-document-rename)
    (sethandler "textDocument/prepareRename" 'clef-lsp/document:handle-text-document-prepare-rename)
    (sethandler "textDocument/signatureHelp" 'clef-lsp/document:handle-text-document-signature-help)
    (sethandler "workspace/diagnostic" 'clef-lsp/workspace:handle-workspace-diagnostic)
    (sethandler "workspace/didChangeConfiguration" 'clef-lsp/workspace:handle-workspace-did-change-configuration)
    (sethandler "workspace/symbol" 'clef-lsp/workspace:handle-workspace-symbol)
    (sethandler "shutdown" 'clef-lsp/misc:handle-shutdown)
    (sethandler "exit" 'clef-lsp/misc:handle-exit)))
