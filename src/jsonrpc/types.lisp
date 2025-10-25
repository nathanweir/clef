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
         ;; Including error here is not accurate; the spec does not allow includin both result and error,
         ;; even when one is null
         ;; (error
         ;;         :initarg :error
         ;;     :accessor response-error
         ;;     :initform nil
         ;;     :type (or jsonrpc-error null)
         ;;     :documentation "Error object (required on error).")
         (id
          :initarg :id
          :accessor response-id
          :type jsonrpc-id
          :documentation "Must match the id from the Request object."))
    (:documentation "JSON-RPC 2.0 Response object. Either result or error must be present, but not both."))

;;; Helper functions for validation

(defun valid-request-p (request)
    "Validate that a request object conforms to JSON-RPC 2.0 specification."
    (and (typep request 'jsonrpc-request)
         (string= (request-jsonrpc request) "2.0")
         (stringp (request-method request))
         (not (and (stringp (request-method request))
                   (string-prefix-p "rpc." (request-method request))))))

(defun valid-response-p (response)
    "Validate that a response object conforms to JSON-RPC 2.0 specification.
    Either result or error must be present, but not both."
    (and (typep response 'jsonrpc-response)
         (string= (response-jsonrpc response) "2.0")
         (slot-boundp response 'id)
         ;; XOR: exactly one of result or error must be present
         (not (eq (slot-boundp response 'result)
                  (slot-boundp response 'error)))))

(defun notification-p (request)
    "Check if a request is a notification (no id member)."
    (and (typep request 'jsonrpc-request)
         (not (slot-boundp request 'id))))
