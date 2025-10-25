(in-package :clef-lsp/types/lifecyle)

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
