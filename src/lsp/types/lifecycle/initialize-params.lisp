(in-package :clef-lsp/types/lifecycle)

(deftype trace-value ()
    "The level of verbosity of the execution trace. https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue"
    '(member "off" "messages" "verbose"))

(defclass workspace-folder ()
        ((uri
          :initarg :uri
          :accessor workspace-folder-uri
          :type document-uri
          :documentation "The associated URI for this workspace folder.")
         (name
          :initarg :name
          :accessor workspace-folder-name
          :type string
          :documentation "The name of the workspace folder. Used to refer to this workspace folder in the user interface.")))


(defclass client-info ()
        ((name
          :initarg :name
          :accessor client-info-name
          :type string
          :documentation "The name of the client as defined by the client.")
         (version
          :initarg :version
          :accessor client-info-version
          :type (or string null)
          :documentation "The client's version as defined by the client.")))

(defclass initialize-params ()
        ((process-id
          :initarg :process-id
          :accessor initialize-params-process-id
          :type (or integer null)
          :documentation "The process Id of the parent process that started the server. Is null if the process has not been started by another process. If the parent process is not alive then the server should exit its process.")
         (client-info
          :initarg :client-info
          :accessor initialize-params-client-info
          :type (or client-info null)
          :documentation "Information about the client. (since 3.15.0)")
         (locale
          :initarg :locale
          :accessor initialize-params-locale
          :type (or string null)
          :documentation "The locale the client is currently showing the user interface in. Uses IETF language tags. (since 3.16.0)")
         (root-path
          :initarg :root-path
          :accessor initialize-params-root-path
          :type (or string null)
          :documentation "The rootPath of the workspace. Is null if no folder is open. Deprecated in favour of rootUri.")
         (root-uri
          :initarg :root-uri
          :accessor initialize-params-root-uri
          :type (or document-uri null)
          :documentation "The rootUri of the workspace. Is null if no folder is open. If both rootPath and rootUri are set, rootUri wins. Deprecated in favour of workspaceFolders.")
         (initialization-options
          :initarg :initialization-options
          :accessor initialize-params-initialization-options
          :type (or lspany null)
          :documentation "User provided initialization options.")
         (capabilities
          :initarg :capabilities
          :accessor initialize-params-capabilities
          :type client-capabilities
          :documentation "The capabilities provided by the client (editor or tool).")
         (trace
             :initarg :trace
             :accessor initialize-params-trace
             :type (or trace-value null)
             :documentation "The initial trace setting. If omitted trace is disabled ('off').")
         (workspace-folders
          :initarg :workspace-folders
          :accessor initialize-params-workspace-folders
          :type (or (vector workspace-folder) null)
          :documentation "The workspace folders configured in the client when the server starts. Only available if the client supports workspace folders. Can be null if none are configured. (since 3.6.0)")))

(defclass client-capabilities ()
        ((workspace
          :initarg :workspace
          :accessor client-capabilities-workspace
          :type (or null client-capabilities-workspace)
          :documentation "Workspace specific client capabilities.")
         ;; TODO: This is huge, so intentionally skipping this for now
         ;; (text-document
         ;;  :initarg :text-document
         ;;  :accessor client-capabilities-text-document
         ;;  :type (or null text-document-client-capabilities)
         ;;  :documentation "Text document specific client capabilities.")
         ;; TODO: To be revisited after evaluating notebook document support
         ;; (notebook-document
         ;;  :initarg :notebook-document
         ;;  :accessor client-capabilities-notebook-document
         ;;  :type (or null notebook-document-client-capabilities)
         ;;  :documentation "Capabilities specific to the notebook document support. (since 3.17.0)")
         (window
          :initarg :window
          :accessor client-capabilities-window
          :type (or null client-capabilities-window)
          :documentation "Window specific client capabilities.")
         (general
          :initarg :general
          :accessor client-capabilities-general
          :type (or null client-capabilities-general)
          :documentation "General client capabilities. (since 3.16.0)")
         (experimental
          :initarg :experimental
          :accessor client-capabilities-experimental
          :type (or null lspany)
          :documentation "Experimental client capabilities.")))

;; Workspace capabilities
(defclass client-capabilities-workspace ()
        ((apply-edit
          :initarg :apply-edit
          :accessor client-capabilities-workspace-apply-edit
          :type (or null boolean)
          :documentation "The client supports applying batch edits to the workspace by supporting the request 'workspace/applyEdit'.")
         (workspace-edit
          :initarg :workspace-edit
          :accessor client-capabilities-workspace-workspace-edit
          :type (or null workspace-edit-client-capabilities)
          :documentation "Capabilities specific to `WorkspaceEdit`s.")
         (did-change-configuration
          :initarg :did-change-configuration
          :accessor client-capabilities-workspace-did-change-configuration
          :type (or null did-change-configuration-client-capabilities)
          :documentation "Capabilities specific to the `workspace/didChangeConfiguration` notification.")
         (did-change-watched-files
          :initarg :did-change-watched-files
          :accessor client-capabilities-workspace-did-change-watched-files
          :type (or null did-change-watched-files-client-capabilities)
          :documentation "Capabilities specific to the `workspace/didChangeWatchedFiles` notification.")
         (symbol
          :initarg :symbol
          :accessor client-capabilities-workspace-symbol
          :type (or null workspace-symbol-client-capabilities)
          :documentation "Capabilities specific to the `workspace/symbol` request.")
         (execute-command
          :initarg :execute-command
          :accessor client-capabilities-workspace-execute-command
          :type (or null execute-command-client-capabilities)
          :documentation "Capabilities specific to the `workspace/executeCommand` request.")
         (workspace-folders
          :initarg :workspace-folders
          :accessor client-capabilities-workspace-workspace-folders
          :type (or null boolean)
          :documentation "The client has support for workspace folders. (since 3.6.0)")
         (configuration
          :initarg :configuration
          :accessor client-capabilities-workspace-configuration
          :type (or null boolean)
          :documentation "The client supports `workspace/configuration` requests. (since 3.6.0)")
         (semantic-tokens
          :initarg :semantic-tokens
          :accessor client-capabilities-workspace-semantic-tokens
          :type (or null semantic-tokens-workspace-client-capabilities)
          :documentation "Capabilities specific to the semantic token requests scoped to the workspace. (since 3.16.0)")
         (code-lens
          :initarg :code-lens
          :accessor client-capabilities-workspace-code-lens
          :type (or null code-lens-workspace-client-capabilities)
          :documentation "Capabilities specific to the code lens requests scoped to the workspace. (since 3.16.0)")
         (file-operations
          :initarg :file-operations
          :accessor client-capabilities-workspace-file-operations
          :type (or null client-capabilities-file-operations)
          :documentation "The client has support for file requests/notifications. (since 3.16.0)")
         (inline-value
          :initarg :inline-value
          :accessor client-capabilities-workspace-inline-value
          :type (or null inline-value-workspace-client-capabilities)
          :documentation "Client workspace capabilities specific to inline values. (since 3.17.0)")
         (inlay-hint
          :initarg :inlay-hint
          :accessor client-capabilities-workspace-inlay-hint
          :type (or null inlay-hint-workspace-client-capabilities)
          :documentation "Client workspace capabilities specific to inlay hints. (since 3.17.0)")
         (diagnostics
          :initarg :diagnostics
          :accessor client-capabilities-workspace-diagnostics
          :type (or null diagnostic-workspace-client-capabilities)
          :documentation "Client workspace capabilities specific to diagnostics. (since 3.17.0)")))

;; File operations
(defclass client-capabilities-file-operations ()
        ((dynamic-registration
          :initarg :dynamic-registration
          :accessor client-capabilities-file-operations-dynamic-registration
          :type (or null boolean)
          :documentation "Whether the client supports dynamic registration for file requests/notifications.")
         (did-create
          :initarg :did-create
          :accessor client-capabilities-file-operations-did-create
          :type (or null boolean)
          :documentation "The client has support for sending didCreateFiles notifications.")
         (will-create
          :initarg :will-create
          :accessor client-capabilities-file-operations-will-create
          :type (or null boolean)
          :documentation "The client has support for sending willCreateFiles requests.")
         (did-rename
          :initarg :did-rename
          :accessor client-capabilities-file-operations-did-rename
          :type (or null boolean)
          :documentation "The client has support for sending didRenameFiles notifications.")
         (will-rename
          :initarg :will-rename
          :accessor client-capabilities-file-operations-will-rename
          :type (or null boolean)
          :documentation "The client has support for sending willRenameFiles requests.")
         (did-delete
          :initarg :did-delete
          :accessor client-capabilities-file-operations-did-delete
          :type (or null boolean)
          :documentation "The client has support for sending didDeleteFiles notifications.")
         (will-delete
          :initarg :will-delete
          :accessor client-capabilities-file-operations-will-delete
          :type (or null boolean)
          :documentation "The client has support for sending willDeleteFiles requests.")))

;; Window capabilities
(defclass client-capabilities-window ()
        ((work-done-progress
          :initarg :work-done-progress
          :accessor client-capabilities-window-work-done-progress
          :type (or null boolean)
          :documentation "Whether the client supports server initiated progress using the `window/workDoneProgress/create` request. (since 3.15.0)")
         (show-message
          :initarg :show-message
          :accessor client-capabilities-window-show-message
          :type (or null show-message-request-client-capabilities)
          :documentation "Capabilities specific to the showMessage request. (since 3.16.0)")
         (show-document
          :initarg :show-document
          :accessor client-capabilities-window-show-document
          :type (or null show-document-client-capabilities)
          :documentation "Client capabilities for the show document request. (since 3.16.0)")))

;; General capabilities
(defclass client-capabilities-general ()
        ((stale-request-support
          :initarg :stale-request-support
          :accessor client-capabilities-general-stale-request-support
          :type (or null client-capabilities-stale-request-support)
          :documentation "Client capability that signals how the client handles stale requests. (since 3.17.0)")
         (regular-expressions
          :initarg :regular-expressions
          :accessor client-capabilities-general-regular-expressions
          :type (or null regular-expressions-client-capabilities)
          :documentation "Client capabilities specific to regular expressions. (since 3.16.0)")
         (markdown
          :initarg :markdown
          :accessor client-capabilities-general-markdown
          :type (or null markdown-client-capabilities)
          :documentation "Client capabilities specific to the client's markdown parser. (since 3.16.0)")
         (position-encodings
          :initarg :position-encodings
          :accessor client-capabilities-general-position-encodings
          :type (or null (vector position-encoding-kind))
          :documentation "The position encodings supported by the client. (since 3.17.0)")))

;; Stale request support
(defclass client-capabilities-stale-request-support ()
        ((cancel
          :initarg :cancel
          :accessor client-capabilities-stale-request-support-cancel
          :type boolean
          :documentation "The client will actively cancel the request.")
         (retry-on-content-modified
          :initarg :retry-on-content-modified
          :accessor client-capabilities-stale-request-support-retry-on-content-modified
          :type (vector string)
          :documentation "The list of requests for which the client will retry the request if it receives a response with error code `ContentModified`.")))

(defclass workspace-edit-client-capabilities ()
        ((document-changes
          :initarg :document-changes
          :accessor workspace-edit-client-capabilities-document-changes
          :type (or boolean null)
          :documentation "The client supports versioned document changes in `WorkspaceEdit`s")
         (resource-operations
          :initarg :resource-operations
          :accessor workspace-edit-client-capabilities-resource-operations
          :type (or (vector resource-operation-kind) null)
          :documentation "The resource operations the client supports. Clients should at least support 'create', 'rename' and 'delete' files and folders. (since 3.13.0)")
         (failure-handling
          :initarg :failure-handling
          :accessor workspace-edit-client-capabilities-failure-handling
          :type (or failure-handling-kind null)
          :documentation "The failure handling strategy of a client if applying the workspace edit fails. (since 3.13.0)")
         (normalizes-line-endings
          :initarg :normalizes-line-endings
          :accessor workspace-edit-client-capabilities-normalizes-line-endings
          :type (or boolean null)
          :documentation "Whether the client normalizes line endings to the client specific setting. If set to `true` the client will normalize line ending characters in a workspace edit to the client specific new line character(s). (since 3.16.0)")
         (change-annotation-support
          :initarg :change-annotation-support
          :accessor workspace-edit-client-capabilities-change-annotation-support
          :type (or workspace-edit-client-capabilities-change-annotation-support null)
          :documentation "Whether the client in general supports change annotations on text edits, create file, rename file and delete file changes. (since 3.16.0)")))

(defclass workspace-edit-client-capabilities-change-annotation-support ()
        ((groups-on-label
          :initarg :groups-on-label
          :accessor workspace-edit-client-capabilities-change-annotation-support-groups-on-label
          :type (or boolean null)
          :documentation "Whether the client groups edits with equal labels into tree nodes, for instance all edits labelled with \"Changes in Strings\" would be a tree node.")))

(deftype resource-operation-kind ()
    "The kind of resource operations supported by the client."
    '(or "create" "rename" "delete"))

(deftype failure-handling-kind ()
    "The failure handling strategy of a client if applying the workspace edit fails."
    '(or "abort" "transactional" "undo" "textOnlyTransactional"))

(defclass did-change-configuration-client-capabilities ()
        ((dynamic-registration
          :initarg :dynamic-registration
          :accessor did-change-configuration-client-capabilities-dynamic-registration
          :type (or boolean null)
          :documentation "Did change configuration notification supports dynamic registration. (since 3.6.0 to support the new pull model.)")))

(defclass did-change-watched-files-client-capabilities ()
        ((dynamic-registration
          :initarg :dynamic-registration
          :accessor did-change-watched-files-client-capabilities-dynamic-registration
          :type (or boolean null)
          :documentation "Did change watched files notification supports dynamic registration. Please note that the current protocol doesn't support static configuration for file changes from the server side.")
         (relative-pattern-support
          :initarg :relative-pattern-support
          :accessor did-change-watched-files-client-capabilities-relative-pattern-support
          :type (or boolean null)
          :documentation "Whether the client has support for relative patterns or not. (since 3.17.0)")))

(defclass did-change-watched-files-client-capabilities ()
        ((dynamic-registration
          :initarg :dynamic-registration
          :accessor did-change-watched-files-client-capabilities-dynamic-registration
          :type (or boolean null)
          :documentation "Did change watched files notification supports dynamic registration. Please note that the current protocol doesn't support static configuration for file changes from the server side.")
         (relative-pattern-support
          :initarg :relative-pattern-support
          :accessor did-change-watched-files-client-capabilities-relative-pattern-support
          :type (or boolean null)
          :documentation "Whether the client has support for relative patterns or not. (since 3.17.0)")))

(defclass workspace-symbol-client-capabilities ()
        ((dynamic-registration
          :initarg :dynamic-registration
          :accessor workspace-symbol-client-capabilities-dynamic-registration
          :type (or boolean null)
          :documentation "Symbol request supports dynamic registration.")
         (symbol-kind
          :initarg :symbol-kind
          :accessor workspace-symbol-client-capabilities-symbol-kind
          :type (or workspace-symbol-client-capabilities-symbol-kind null)
          :documentation "Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.")
         (tag-support
          :initarg :tag-support
          :accessor workspace-symbol-client-capabilities-tag-support
          :type (or workspace-symbol-client-capabilities-tag-support null)
          :documentation "The client supports tags on `SymbolInformation` and `WorkspaceSymbol`. Clients supporting tags have to handle unknown tags gracefully. (since 3.16.0)")
         (resolve-support
          :initarg :resolve-support
          :accessor workspace-symbol-client-capabilities-resolve-support
          :type (or workspace-symbol-client-capabilities-resolve-support null)
          :documentation "The client support partial workspace symbols. The client will send the request `workspaceSymbol/resolve` to the server to resolve additional properties. (since 3.17.0 - proposedState)")))

(defclass workspace-symbol-client-capabilities-symbol-kind ()
        ((value-set
          :initarg :value-set
          :accessor workspace-symbol-client-capabilities-symbol-kind-value-set
          :type (or (vector symbol-kind) null)
          :documentation "The symbol kind values the client supports. When this property exists the client also guarantees that it will handle values outside its set gracefully and falls back to a default value when unknown. If this property is not present the client only supports the symbol kinds from `File` to `Array` as defined in the initial version of the protocol.")))

(defclass workspace-symbol-client-capabilities-tag-support ()
        ((value-set
          :initarg :value-set
          :accessor workspace-symbol-client-capabilities-tag-support-value-set
          :type (vector symbol-tag)
          :documentation "The tags supported by the client.")))

(defclass workspace-symbol-client-capabilities-resolve-support ()
        ((properties
          :initarg :properties
          :accessor workspace-symbol-client-capabilities-resolve-support-properties
          :type (vector string)
          :documentation "The properties that a client can resolve lazily. Usually `location.range`")))

(defclass execute-command-client-capabilities ()
        ((dynamic-registration
          :initarg :dynamic-registration
          :accessor execute-command-client-capabilities-dynamic-registration
          :type (or boolean null)
          :documentation "Execute command supports dynamic registration.")))

(defclass semantic-tokens-workspace-client-capabilities ()
        ((refresh-support
          :initarg :refresh-support
          :accessor semantic-tokens-workspace-client-capabilities-refresh-support
          :type (or boolean null)
          :documentation "Whether the client implementation supports a refresh request sent from the server to the client. Note that this event is global and will force the client to refresh all semantic tokens currently shown. It should be used with absolute care and is useful for situation where a server for example detect a project wide change that requires such a calculation.")))

(defclass code-lens-workspace-client-capabilities ()
        ((refresh-support
          :initarg :refresh-support
          :accessor code-lens-workspace-client-capabilities-refresh-support
          :type (or boolean null)
          :documentation "Whether the client implementation supports a refresh request sent from the server to the client. Note that this event is global and will force the client to refresh all code lenses currently shown. It should be used with absolute care and is useful for situation where a server for example detect a project wide change that requires such a calculation.")))

(defclass inline-value-workspace-client-capabilities ()
        ((refresh-support
          :initarg :refresh-support
          :accessor inline-value-workspace-client-capabilities-refresh-support
          :type (or boolean null)
          :documentation "Whether the client implementation supports a refresh request sent from the server to the client. Note that this event is global and will force the client to refresh all inline values currently shown. It should be used with absolute care and is useful for situation where a server for example detect a project wide change that requires such a calculation. (since 3.17.0)")))

(defclass inlay-hint-workspace-client-capabilities ()
        ((refresh-support
          :initarg :refresh-support
          :accessor inlay-hint-workspace-client-capabilities-refresh-support
          :type (or boolean null)
          :documentation "Whether the client implementation supports a refresh request sent from the server to the client. Note that this event is global and will force the client to refresh all inlay hints currently shown. It should be used with absolute care and is useful for situation where a server for example detects a project wide change that requires such a calculation. (since 3.17.0)")))

(defclass diagnostic-workspace-client-capabilities ()
        ((refresh-support
          :initarg :refresh-support
          :accessor diagnostic-workspace-client-capabilities-refresh-support
          :type (or boolean null)
          :documentation "Whether the client implementation supports a refresh request sent from the server to the client. Note that this event is global and will force the client to refresh all pulled diagnostics currently shown. It should be used with absolute care and is useful for situation where a server for example detects a project wide change that requires such a calculation. (since 3.17.0)")))

(defclass regular-expressions-client-capabilities ()
        ((engine
          :initarg :engine
          :accessor regular-expressions-client-capabilities-engine
          :type string
          :documentation "The engine's name.")
         (version
          :initarg :version
          :accessor regular-expressions-client-capabilities-version
          :type (or string null)
          :documentation "The engine's version.")))

(defclass markdown-client-capabilities ()
        ((parser
          :initarg :parser
          :accessor markdown-client-capabilities-parser
          :type string
          :documentation "The name of the parser.")
         (version
          :initarg :version
          :accessor markdown-client-capabilities-version
          :type (or string null)
          :documentation "The version of the parser.")
         (allowed-tags
          :initarg :allowed-tags
          :accessor markdown-client-capabilities-allowed-tags
          :type (or (vector string) null)
          :documentation "A list of HTML tags that the client allows / supports in Markdown. (since 3.17.0)")))
