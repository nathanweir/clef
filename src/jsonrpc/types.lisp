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

;; TODO: if good, move to a util
;; This is 100%, pure LLM slop. Need to look at common util libraries for more appopriate solutions
;; (defun pprint-dict (obj stream &optional (indent 0))
;;     (cond
;;      ((hash-table-p obj)
;;          (format stream "{~%")
;;          (let ((keys (sort (loop for k being the hash-keys of obj collect k)
;;                              #'string<)))
;;              (dolist (k keys)
;;                  (format stream "~V@T~S: " (+ indent 2) k)
;;                  (pprint-dict (gethash k obj) stream (+ indent 2))))
;;          (format stream "~V@T}~%" indent))
;;      ((stringp obj)
;;          (format stream "~V@T~S~%" indent obj))
;;      ((vectorp obj)
;;          (format stream "[~%")
;;          (dotimes (i (length obj))
;;              (format stream "~V@T" (+ indent 2))
;;              (pprint-dict (aref obj i) stream (+ indent 2))
;;              (when (< i (1- (length obj)))
;;                    (format stream "~%")))
;;          (format stream "~V@T]~%" indent))
;;      ((listp obj)
;;          (format stream "[~%")
;;          (loop for item in obj
;;                for i from 0
;;                do (format stream "~V@T" (+ indent 2))
;;                    (pprint-dict item stream (+ indent 2))
;;                    when (< i (1- (length obj)))
;;                do (format stream "~%"))
;;          (format stream "~V@T]~%" indent))
;;      (t
;;          (format stream "~S~%" obj))))

(defun pprint-indented (level)
    "Return indentation string for the given nesting level."
    (make-string (* 2 level) :initial-element #\Space))

(defun pprint-value (value stream level)
    "Pretty-print a single value with appropriate indentation."
    (cond
     ((null value)
         (format stream "nil"))
     ((stringp value)
         (format stream "\"~A\"" value))
     ((or (numberp value) (eq value t) (eq value nil))
         (format stream "~A" value))
     ((hash-table-p value)
         (pprint-dict value stream level))
     ((vectorp value)
         (pprint-vector value stream level))
     ((listp value)
         (pprint-list value stream level))
     (t
         (format stream "~A" value))))

(defun pprint-vector (vec stream level)
    "Pretty-print a vector with proper indentation."
    (if (zerop (length vec))
        (format stream "[]")
        (progn
         (format stream "[~%")
         (let ((len (length vec)))
             (loop for i from 0 below len
                   for item = (aref vec i)
                   do (progn
                       (format stream "~A" (pprint-indented (+ level 1)))
                       (pprint-value item stream (+ level 1))
                       (if (< i (- len 1))
                           (format stream ",~%")
                           ;; This comma makes this if redundant
                           ;; TODO: All of these format funcs are LLM slop
                           (format stream ",~%"))))) ; newline after last element
         (format stream "~A]" (pprint-indented level)))))

(defun pprint-dict (dict stream level)
    "Pretty-print a hash table as a dictionary with proper indentation."
    (format stream "{~%")
    (let* ((keys (loop for key being the hash-keys of dict collect key))
           (sorted-keys (sort keys #'string< :key #'string)))
        (loop for i from 0
              for key in sorted-keys
              do (progn
                  (format stream "~A\"~A\": " (pprint-indented (+ level 1)) key)
                  (pprint-value (gethash key dict) stream (+ level 1))
                  (if (< i (- (length sorted-keys) 1))
                      (format stream ",~%")
                      (format stream "~%")))))
    (format stream "~A}" (pprint-indented level)))

(defun pprint-list (lst stream level)
    "Pretty-print a list as an array with proper indentation."
    (if (null lst)
        (format stream "[]")
        (progn
         (format stream "[~%")
         (let ((len (length lst)))
             (loop for i from 0
                   for item in lst
                   do (progn
                       (format stream "~A" (pprint-indented (+ level 1)))
                       (pprint-value item stream (+ level 1))
                       (if (< i (- len 1))
                           (format stream ",~%")
                           (format stream "~%")))))
         (format stream "~A]" (pprint-indented level)))))

;; Enable this only when you need deep printing of the request object
;; (defmethod print-object ((obj jsonrpc-request) stream)
;;     (format stream "#<jsonrpc-request jsonrpc=~A method=~A id=~A>~%"
;;         (request-jsonrpc obj)
;;         (request-method obj)
;;         (request-id obj))
;;     (let ((params (request-params obj)))
;;         (format stream "params=")
;;         (pprint-value params stream 0))
;;     (format stream "~%"))

;; (defmethod print-object ((obj jsonrpc-request) stream)
;;     (format stream "#<jsonrpc-request jsonrpc=~A method=~A id=~A>~%"
;;         (request-jsonrpc obj)
;;         (request-method obj)
;;         (request-id obj))
;;     (let ((params (request-params obj)))
;;         (format stream "params=")
;;         (if params
;;             (pprint-dict params stream 0)
;;             (format stream "nil~%"))))

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

;;; Helper functions for validation

;; (defun valid-request-p (request)
;;     "Validate that a request object conforms to JSON-RPC 2.0 specification."
;;     (and (typep request 'jsonrpc-request)
;;          (string= (request-jsonrpc request) "2.0")
;;          (stringp (request-method request))
;;          (not (and (stringp (request-method request))
;;                    (string-prefix-p "rpc." (request-method request))))))

;; (defun valid-response-p (response)
;;     "Validate that a response object conforms to JSON-RPC 2.0 specification.
;;     Either result or error must be present, but not both."
;;     (and (typep response 'jsonrpc-response)
;;          (string= (response-jsonrpc response) "2.0")
;;          (slot-boundp response 'id)
;;          ;; XOR: exactly one of result or error must be present
;;          (not (eq (slot-boundp response 'result)
;;                   (slot-boundp response 'error)))))

;; (defun notification-p (request)
;;     "Check if a request is a notification (no id member)."
;;     (and (typep request 'jsonrpc-request)
;;          (not (slot-boundp request 'id))))
