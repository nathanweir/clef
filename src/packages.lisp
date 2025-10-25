(defpackage :clef-root
    (:use :cl)
    (:export :start-server))

(defpackage :clef-jsonrpc/types
    (:use :cl)
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
             request-id
             jsonrpc-response
             valid-request-p
             valid-response-p
             notification-p))

(defpackage :clef-jsonrpc/messages
    (:use :cl)
    (:import-from :clef-jsonrpc/types :jsonrpc-request :jsonrpc-response)
    (:export :read-lsp-message
             :write-lsp-message))

(defpackage :clef-lsp/server
    (:use :cl)
    (:export :start
             ;; :before-handle-request
             *server*))

(defpackage :clef-lsp/defhandler
    (:use :cl)
    ;; I have no idea why, but I *must* import *server* here
    ;; and use without the fully qualified name in defhandler in order to
    ;; properly reference it
    ;; (:import-from :clef-lsp/server *server* :before-handle-request)
    (:export :defhandler))

(defpackage :clef-lsp/types/base
    (:use :cl)
    (:export :uinteger
             +server-not-initialized+))

(defpackage :clef-lsp/types/basic
    (:use :cl)
    (:import-from :clef-lsp/types/base :uinteger)
    ;; TODO: Just how dangerous is this?
    (:shadow :position)
    (:export :position
             :position-line
             :position-character))

(defpackage :clef-lsp/lifecycle
    (:use :cl :clef-lsp/defhandler))
