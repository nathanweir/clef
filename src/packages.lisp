(defpackage :clef-util
            (:use :cl)
            (:export :hash-table-to-instance
                     :shallow-hash-vals
                     :cleanup-path
                     :read-file-text))

(defpackage :clef-log
            (:use :cl)
            (:export :slog
                     *log-level*
                     *log-levels*
                     *log-mode*
                     *log-file-path*
                     init))

(defpackage :clef-context
            (:use :cl)
            (:documentation "Central server context. Holds all persistent CLEF LSP
state on a single SERVER-CONTEXT struct bound to *SERVER*. All other CLEF
packages reach shared state through the accessors exported here rather than
through their own defparameters.")
            (:export :server-context
                     :make-server-context
                     :server-context-p
                     :*server*
                     :reset-context
                     ;; Struct field accessors (the generated ones, for when the
                     ;; symbol-macro aliases can't be used)
                     :server-context-initialized
                     :server-context-shutdown-received
                     :server-context-client-capabilities
                     :server-context-workspace-root
                     :server-context-output-stream
                     :server-context-handlers
                     :server-context-documents
                     :server-context-lexical-scopes
                     :server-context-symbol-refs
                     :server-context-workspace-symbol-index
                     :server-context-document-line-offsets
                     :server-context-global-scope
                     :server-context-loaded-systems
                     :server-context-file-to-system
                     :server-context-asd-files
                     ;; Symbol-macro aliases (short form, preferred at call sites)
                     :initialized
                     :shutdown-received
                     :client-capabilities
                     :workspace-root
                     :output-stream
                     :handlers
                     :documents
                     :lexical-scopes
                     :symbol-refs
                     :workspace-symbol-index
                     :document-line-offsets
                     :global-scope
                     :loaded-systems
                     :file-to-system
                     :asd-files))

(defpackage :clef-root
            (:use :cl :clef-log)
            (:export :start-server))

(defpackage :clef-jsonrpc/types
            (:use :cl :clef-log)
            (:export +parse-error+
                     +invalid-request+
                     +method-not-found+
                     +invalid-params+
                     +internal-error+
                     jsonrpc-id
                     jsonrpc-params
                     jsonrpc-data
                     jsonrpc-error
                     jsonrpc-request
                     hash-table-to-request
                     request-id
                     request-method
                     request-params
                     jsonrpc-response
                     jsonrpc-error-response
                     valid-request-p
                     valid-response-p
                     notification-p))

(defpackage :clef-jsonrpc/messages
            (:use :cl :clef-log)
            (:import-from :clef-jsonrpc/types :jsonrpc-request :jsonrpc-response)
            (:export :read-lsp-message
                     :write-lsp-message))

(defpackage :clef-parser/parser
            (:use :cl :clef-log)
            (:export :parse-file
                     :parse-string
                     :node-start-point-row
                     :node-start-point-column
                     :node-end-point-row
                     :node-end-point-column
                     :node-text
                     :node-range))

(defpackage :clef-parser/utils
            (:use :cl :clef-log)
            (:local-nicknames
              (:ts :cl-tree-sitter/high-level))
            (:export :find-package-declaration))

(defpackage :clef-symbols
            (:use :cl :clef-log :clef-parser/parser)
            (:local-nicknames
              (:ctx :clef-context)
              (:ts :cl-tree-sitter/high-level)
              (:ts-ll :cl-tree-sitter/low-level))
            (:export build-project-symbol-map
                     build-file-symbol-map
                     get-ref-for-doc-pos
                     lexical-scope-kind
                     lexical-scope-symbol-definitions
                     lexical-scope-parent-scope
                     lexical-scope-location
                     symbol-definition-symbol-name
                     symbol-definition-location
                     symbol-definition-defining-scope
                     location-file-path
                     location-start
                     location-end
                     lexical-scope-node
                     symbol-definition-node
                     symbol-definition-kind
                     symbol-reference-node
                     symbol-reference-symbol-name
                     ;; system-info struct and accessors
                     system-info
                     make-system-info
                     system-info-name
                     system-info-asd-path
                     system-info-dependencies
                     system-info-source-files
                     system-info-loaded-p
                     ;; Workspace symbol index management (operates on context)
                     clear-workspace-symbol-index
                     remove-file-from-workspace-index
                     add-to-workspace-index
                     lookup-in-workspace-index
                     ;; Byte offset helpers (used by some handlers)
                     line-char-to-byte-offset))

(defpackage :clef-lsp/server
            (:use :cl :clef-log)
            (:local-nicknames
              (:ctx :clef-context))
            (:import-from :serapeum :dict)
            (:export :start
                     :sethandler
                     :register-handlers
                     :before-handle-request
                     :handle-lsp-request
                     :send-notification
                     :publish-diagnostics
                     :reset
                     *server-capabilities-json*))

(defpackage :clef-lsp/types/base
            (:use :cl :clef-log)
            (:export :uinteger
                     :document-uri
                     +server-not-initialized+
                     server-not-initialized-error
                     method-not-found-error
                     :lsp-error
                     :lsp-error-code
                     :lsp-error-message
                     :lsp-error-data
                     ;; CompletionItemKind constants
                     +completion-item-kind-text+
                     +completion-item-kind-method+
                     +completion-item-kind-function+
                     +completion-item-kind-constructor+
                     +completion-item-kind-field+
                     +completion-item-kind-variable+
                     +completion-item-kind-class+
                     +completion-item-kind-interface+
                     +completion-item-kind-module+
                     +completion-item-kind-property+
                     +completion-item-kind-unit+
                     +completion-item-kind-value+
                     +completion-item-kind-enum+
                     +completion-item-kind-keyword+
                     +completion-item-kind-snippet+
                     +completion-item-kind-color+
                     +completion-item-kind-file+
                     +completion-item-kind-reference+
                     +completion-item-kind-folder+
                     +completion-item-kind-enum-member+
                     +completion-item-kind-constant+
                     +completion-item-kind-struct+
                     +completion-item-kind-event+
                     +completion-item-kind-operator+
                     +completion-item-kind-type-parameter+))

(defpackage :clef-lsp/types/basic
            (:use :cl :clef-log)
            (:import-from :clef-lsp/types/base :uinteger)
            ;; TODO: Just how dangerous is this?
            (:shadow :position)
            (:export :position
                     :position-line
                     :position-character))

(defpackage :clef-lsp/lifecycle
            (:use :cl :clef-log)
            (:local-nicknames
              (:ctx :clef-context))
            (:import-from :serapeum :dict :href)
            (:export handle-initialize
                     handle-initialized
                     ;; Legacy - kept for backward compatibility
                     load-workspace-asd
                     load-asd
                     ;; Multi-ASD support
                     discover-asd-files
                     load-all-workspace-systems
                     get-file-system
                     list-workspace-systems
                     parse-asd-file
                     load-system-with-info
                     build-file-to-system-mapping))

(defpackage :clef-lsp/document
            (:use :cl :clef-log :clef-symbols)
            (:local-nicknames
              (:ctx :clef-context)
              (:ts :cl-tree-sitter/high-level))
            (:import-from :serapeum :dict :href)
            (:export
              handle-text-document-completion
              handle-text-document-definition
              handle-text-document-references
              handle-text-document-diagnostic
              handle-text-document-did-open
              handle-text-document-did-change
              handle-text-document-did-save
              handle-text-document-formatting
              handle-text-document-hover
              handle-text-document-highlight
              handle-text-document-signature-help))

(defpackage :clef-lsp/workspace
            (:use :cl :clef-log)
            (:local-nicknames
              (:ctx :clef-context))
            (:import-from :serapeum :dict :href)
            (:export handle-workspace-diagnostic
                     handle-workspace-did-change-configuration
                     handle-workspace-symbol))

(defpackage :clef-lsp/misc
            (:use :cl :clef-log)
            (:local-nicknames
              (:ctx :clef-context))
            (:import-from :serapeum :dict)
            (:export handle-shutdown
                     handle-exit))
