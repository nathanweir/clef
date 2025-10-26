(in-package :clef-lsp/types/base)

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#errorCodes

;; Defined by JSON-RPC
;; TODO: Not sure these are valuable to have here as the :jsonrpc lib
;; *should* handle these


(defconstant +jsonrpc-reserved-error-range-start+ -32099
             "Start of JSON-RPC reserved error codes. Not a real error code. No LSP error codes should be defined between the start and end range. For backwards compatibility, +server-not-initialized+ and +unknown-error-code+ are left in the range. (since 3.16.0)")

(defconstant +server-error-start+ +jsonrpc-reserved-error-range-start+
             "Deprecated: use +jsonrpc-reserved-error-range-start+.")

(defconstant +server-not-initialized+ -32002
             "Error code indicating that a server received a notification or request before the server received the `initialize` request.")

(defconstant +unknown-error-code+ -32001
             "Unknown error code.")

(defconstant +jsonrpc-reserved-error-range-end+ -32000
             "End of JSON-RPC reserved error codes. Not a real error code. (since 3.16.0)")

(defconstant +server-error-end+ +jsonrpc-reserved-error-range-end+
             "Deprecated: use +jsonrpc-reserved-error-range-end+.")

(defconstant +lsp-reserved-error-range-start+ -32899
             "Start of LSP reserved error codes. Not a real error code. (since 3.16.0)")

(defconstant +request-failed+ -32803
             "A request failed but it was syntactically correct. The error message should contain human readable information about why the request failed. (since 3.17.0)")

(defconstant +server-cancelled+ -32802
             "The server cancelled the request. Only for requests that explicitly support being server cancellable. (since 3.17.0)")

(defconstant +content-modified+ -32801
             "The server detected that the content of a document got modified outside normal conditions.")

(defconstant +request-cancelled+ -32800
             "The client has canceled a request and a server has detected the cancel.")

(defconstant +lsp-reserved-error-range-end+ -32800
             "End of LSP reserved error codes. Not a real error code. (since 3.16.0)")

;; TODO: Learn more about Common Lisp error conds and consider exposing these instead of the
;; constants or rename this file
(define-condition lsp-error (error)
        ((code :initarg :code
               :accessor lsp-error-code
               :type integer
               :documentation "The LSP error code.")
         (message :initarg :message
                  :accessor lsp-error-message
                  :type string
                  :documentation "A human-readable error message.")
         (data :initarg :data
               :accessor lsp-error-data
               :initform nil
               :type t
               :documentation "Additional data about the error (optional).")))

(define-condition server-not-initialized-error (lsp-error)
        ((code :initform +server-not-initialized+)
         (message :initform "The LSP has not yet been initialized.")))

(define-condition method-not-found-error (lsp-error)
        ((code :initform 32601)
         (endpoint :initarg :endpoint :reader method-not-found-endpoint)
         (message :initform "Method not found."))
    ;; TODO: How to handle :message and give it the param / custom message instead?
    (:report (lambda (c s)
                 (format s "The requested endpoint ~A does not exist"
                     (method-not-found-endpoint c)))))
