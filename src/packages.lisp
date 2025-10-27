(defpackage :clef-util
    (:use :cl)
    (:export :hash-table-to-instance))

(defpackage :clef-log
    (:use :cl)
    (:export :slog
             +log-level+
             *log-levels*
             +log-mode+
             +log-file-path+
             init))

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

(defpackage :clef-lsp/server
    (:use :cl :clef-log)
    (:export :start
             *handlers*
             :before-handle-request
             *server*))

;; (defpackage :clef-lsp/defhandler
;;     (:use :cl :clef-log)
;;     (:import-from :clef-lsp/server *handlers* :before-handle-request)
;;     (:export :defhandler))

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
             :lsp-error-data))

(defpackage :clef-lsp/types/basic
    (:use :cl :clef-log)
    (:import-from :clef-lsp/types/base :uinteger)
    ;; TODO: Just how dangerous is this?
    (:shadow :position)
    (:export :position
             :position-line
             :position-character))

(defpackage :clef-lsp/types/lifecycle
    (:use :cl :clef-lsp/types/base :schemata)
    (:export :initialize-params
             :initialize-params-process-id
             :initialize-params-root-uri
             :initialize-params-capabilities
             :workspace-folder
             :client-capabilities))

(defpackage :clef-lsp/lifecycle
    (:use :cl :clef-log :schemata)
    (:export handle-initialize))
