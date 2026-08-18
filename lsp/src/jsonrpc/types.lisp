(in-package :clef-jsonrpc/types)

(defconstant +parse-error+ -32700
             "Parse error (JSON-RPC).")
(defconstant +invalid-request+ -32600
             "Invalid request (JSON-RPC).")
(defconstant +method-not-found+ -32601
             "Method not found (JSON-RPC).")
(defconstant +invalid-params+ -32602
             "Invalid params (JSON-RPC).")
(defconstant +internal-error+ -32603
             "Internal error (JSON-RPC).")

;;; Type definitions for JSON-RPC 2.0

;; JSON-RPC ID can be a string, number, or null
(deftype jsonrpc-id ()
    '(or string number null))

;; Params can be a structured value (list, hash-table, etc.) or omitted
(deftype jsonrpc-params ()
    '(or list hash-table null))

;; Data in error object can be any primitive or structured value
(deftype jsonrpc-data ()
    't)


(defclass jsonrpc-error ()
        ((code
          :initarg :code
          :accessor error-code
          :type integer
          :documentation "An integer indicating the error type that occurred.")
         (message
          :initarg :message
          :accessor error-message
          :type string
          :documentation "A string providing a short description of the error.")
         (data
          :initarg :data
          :accessor error-data
          :initform nil
          :type jsonrpc-data
          :documentation "Additional information about the error (optional)."))
    (:documentation "JSON-RPC 2.0 Error object."))

(defclass jsonrpc-request ()
        ((jsonrpc
          :initarg :jsonrpc
          :accessor request-jsonrpc
          :initform "2.0"
          :type string
          :documentation "JSON-RPC protocol version. Must be exactly \"2.0\".")
         (method
          :initarg :method
          :accessor request-method
          :type string
          :documentation "Name of the method to be invoked.")
         (params
          :initarg :params
          :accessor request-params
          :initform nil
          :type jsonrpc-params
          :documentation "Parameter values for the method invocation (optional).")
         (id
          :initarg :id
          :accessor request-id
          :initform nil
          :type jsonrpc-id
          :documentation "Client-established identifier (optional for notifications)."))
    (:documentation "JSON-RPC 2.0 Request object."))

(defun hash-table-to-request (hash-table params-class)
    "Creates an instance of jsonrpc-request where PARAMS is an instance of REQUEST-PARAMS"
    (make-instance 'jsonrpc-request
        :jsonrpc (gethash "jsonrpc" hash-table)
        :method (gethash "method" hash-table)
        :params (clef-util:hash-table-to-instance (gethash "params" hash-table) params-class)
        :id (gethash "id" hash-table)))

(defclass jsonrpc-response ()
        ((jsonrpc
          :initarg :jsonrpc
          :accessor response-jsonrpc
          :initform "2.0"
          :type string
          :documentation "JSON-RPC protocol version. Must be exactly \"2.0\".")
         (result
          :initarg :result
          :accessor response-result
          :initform nil
          :documentation "Result of the method invocation (required on success).")
         (id
          :initarg :id
          :accessor response-id
          :type jsonrpc-id
          :documentation "Must match the id from the Request object."))
    (:documentation "JSON-RPC 2.0 Response object with a result."))

(defclass jsonrpc-error-response ()
        ((jsonrpc
          :initarg :jsonrpc
          :accessor response-jsonrpc
          :initform "2.0"
          :type string
          :documentation "JSON-RPC protocol version. Must be exactly \"2.0\".")
         ;; This jsonrpc-error-response type exists in contrast to jsonrpc-response as these responses
         ;; cannot contain both a result and an error, even if one of them is null.
         (error
                 :initarg :error
             :accessor response-error
             :initform nil
             :type (or jsonrpc-error null)
             :documentation "Error object (required on error).")
         (id
          :initarg :id
          :accessor response-id
          :type jsonrpc-id
          :documentation "Must match the id from the Request object."))
    (:documentation "JSON-RPC 2.0 Response object with an error."))
