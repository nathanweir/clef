(defpackage :clef-lsp/src/lsp/handlers
  (:use :cl)
  (:local-nicknames
    (:call-hierarchy :clef-lsp/src/lsp/document/call-hierarchy)
    (:code-lens :clef-lsp/src/lsp/document/code-lens)
    (:completion :clef-lsp/src/lsp/document/completion)
    (:definition :clef-lsp/src/lsp/document/definition)
    (:diagnostic :clef-lsp/src/lsp/document/diagnostic)
    (:did-change :clef-lsp/src/lsp/document/did-change)
    (:did-open :clef-lsp/src/lsp/document/did-open)
    (:did-save :clef-lsp/src/lsp/document/did-save)
    (:document-symbol :clef-lsp/src/lsp/document/document-symbol)
    (:exit :clef-lsp/src/lsp/misc/exit)
    (:folding-range :clef-lsp/src/lsp/document/folding-range)
    (:formatting :clef-lsp/src/lsp/document/formatting)
    (:highlight :clef-lsp/src/lsp/document/highlight)
    (:hover :clef-lsp/src/lsp/document/hover)
    (:implementation :clef-lsp/src/lsp/document/implementation)
    (:initialize :clef-lsp/src/lsp/lifecycle/initialize)
    (:initialized :clef-lsp/src/lsp/lifecycle/initialized)
    (:inlay-hint :clef-lsp/src/lsp/document/inlay-hint)
    (:references :clef-lsp/src/lsp/document/references)
    (:rename :clef-lsp/src/lsp/document/rename)
    (:selection-range :clef-lsp/src/lsp/document/selection-range)
    (:semantic-tokens :clef-lsp/src/lsp/document/semantic-tokens)
    (:server :clef-lsp/src/lsp/server)
    (:shutdown :clef-lsp/src/lsp/misc/shutdown)
    (:signature-help :clef-lsp/src/lsp/document/signature-help)
    (:ws-config :clef-lsp/src/lsp/workspace/did-change-configuration)
    (:ws-diagnostic :clef-lsp/src/lsp/workspace/diagnostic)
    (:ws-symbol :clef-lsp/src/lsp/workspace/symbol))
  (:export
   #:register-handlers))

(in-package :clef-lsp/src/lsp/handlers)

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
  (flet ((sethandler (name fn) (server:sethandler name fn)))
    (sethandler "initialize" 'initialize:handle-initialize)
    (sethandler "initialized" 'initialized:handle-initialized)
    (sethandler "textDocument/completion" 'completion:handle-text-document-completion)
    (sethandler "textDocument/definition" 'definition:handle-text-document-definition)
    (sethandler "textDocument/references" 'references:handle-text-document-references)
    (sethandler "textDocument/didOpen" 'did-open:handle-text-document-did-open)
    (sethandler "textDocument/didChange" 'did-change:handle-text-document-did-change)
    (sethandler "textDocument/didClose" 'did-open:handle-text-document-did-close)
    (sethandler "textDocument/didSave" 'did-save:handle-text-document-did-save)
    (sethandler "textDocument/formatting" 'formatting:handle-text-document-formatting)
    (sethandler "textDocument/diagnostic" 'diagnostic:handle-text-document-diagnostic)
    (sethandler "textDocument/hover" 'hover:handle-text-document-hover)
    (sethandler "textDocument/documentHighlight" 'highlight:handle-text-document-highlight)
    (sethandler "textDocument/documentSymbol" 'document-symbol:handle-text-document-document-symbol)
    (sethandler "textDocument/prepareCallHierarchy" 'call-hierarchy:handle-text-document-prepare-call-hierarchy)
    (sethandler "callHierarchy/incomingCalls" 'call-hierarchy:handle-call-hierarchy-incoming-calls)
    (sethandler "callHierarchy/outgoingCalls" 'call-hierarchy:handle-call-hierarchy-outgoing-calls)
    (sethandler "textDocument/implementation" 'implementation:handle-text-document-implementation)
    (sethandler "textDocument/foldingRange" 'folding-range:handle-text-document-folding-range)
    (sethandler "textDocument/selectionRange" 'selection-range:handle-text-document-selection-range)
    (sethandler "textDocument/semanticTokens/full" 'semantic-tokens:handle-text-document-semantic-tokens-full)
    (sethandler "textDocument/inlayHint" 'inlay-hint:handle-text-document-inlay-hint)
    (sethandler "textDocument/codeLens" 'code-lens:handle-text-document-code-lens)
    (sethandler "textDocument/rename" 'rename:handle-text-document-rename)
    (sethandler "textDocument/prepareRename" 'rename:handle-text-document-prepare-rename)
    (sethandler "textDocument/signatureHelp" 'signature-help:handle-text-document-signature-help)
    (sethandler "workspace/diagnostic" 'ws-diagnostic:handle-workspace-diagnostic)
    (sethandler "workspace/didChangeConfiguration" 'ws-config:handle-workspace-did-change-configuration)
    (sethandler "workspace/symbol" 'ws-symbol:handle-workspace-symbol)
    (sethandler "shutdown" 'shutdown:handle-shutdown)
    (sethandler "exit" 'exit:handle-exit)))
