(in-package :clef-test)

;;; Test registry and results

(defparameter *tests* (make-hash-table :test 'equal)
  "Registry of all defined tests")

(defparameter *test-results* nil
  "Results from the last test run")

(defparameter *current-test-name* nil
  "Name of the currently running test")

(defmacro deftest (name &body body)
  "Define a test function and register it"
  `(progn
     (defun ,name ()
       (let ((*current-test-name* ',name))
         ,@body))
     (setf (gethash (symbol-name ',name) *tests*) #',name)))

;;; Assertions

(define-condition test-failure (error)
  ((message :initarg :message :reader test-failure-message)
   (expected :initarg :expected :reader test-failure-expected :initform nil)
   (actual :initarg :actual :reader test-failure-actual :initform nil))
  (:report (lambda (c stream)
             (format stream "Test assertion failed: ~A" (test-failure-message c))
             (when (test-failure-expected c)
               (format stream "~%  Expected: ~S" (test-failure-expected c)))
             (when (test-failure-actual c)
               (format stream "~%  Actual: ~S" (test-failure-actual c))))))

(defun assert-equal (expected actual &optional message)
  "Assert that expected equals actual"
  (unless (equal expected actual)
    (error 'test-failure
           :message (or message (format nil "Expected ~S to equal ~S" actual expected))
           :expected expected
           :actual actual)))

(defun assert-true (value &optional message)
  "Assert that value is truthy"
  (unless value
    (error 'test-failure
           :message (or message "Expected truthy value, got NIL"))))

(defun assert-not-nil (value &optional message)
  "Assert that value is not NIL"
  (when (null value)
    (error 'test-failure
           :message (or message "Expected non-NIL value"))))

(defun assert-nil (value &optional message)
  "Assert that value is NIL"
  (unless (null value)
    (error 'test-failure
           :message (or message (format nil "Expected NIL, got ~S" value)))))

(defun assert-hash-has-key (hash key &optional message)
  "Assert that hash table has the given key"
  (unless (gethash key hash)
    (error 'test-failure
           :message (or message (format nil "Expected hash table to have key ~S" key)))))

(defun response-result-safe (response)
  "Get the result from a response, returning NIL for error responses"
  (typecase response
    (clef-jsonrpc/types:jsonrpc-response
     (clef-jsonrpc/types::response-result response))
    (clef-jsonrpc/types:jsonrpc-error-response
     nil)
    (t nil)))

(defun response-is-error-p (response)
  "Check if response is an error response"
  (typep response 'clef-jsonrpc/types:jsonrpc-error-response))

(defun response-is-success-p (response)
  "Check if response is a successful (non-error) response."
  (typep response 'clef-jsonrpc/types:jsonrpc-response))

(defun answered-p (response)
  "Did the server reply at all?

Deliberately distinct from RESPONSE-RESULT-SAFE, which flattens three different
outcomes onto NIL: no reply, an error reply, and a reply carrying a null result.
That conflation is exactly why the suite could not see the bug where requests
went entirely unanswered -- a test asserting NIL passed whether the server
answered correctly or never answered at all. See docs/surveys/lsp-review.md §1.1.

Assert on this when what you mean is \"the server answered\"; assert on
RESPONSE-RESULT-SAFE when what you mean is \"and the answer was empty\"."
  (not (null response)))

;;; LSP Message utilities

(defun make-lsp-request (method params &key (id 1))
  "Create an LSP request hash table"
  (let ((request (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" request) "2.0")
    (setf (gethash "method" request) method)
    (when params
      (setf (gethash "params" request) params))
    (when id
      (setf (gethash "id" request) id))
    request))

(defun encode-lsp-message (request)
  "Encode an LSP request to bytes with Content-Length header"
  (let* ((json (com.inuoe.jzon:stringify request))
         (body-bytes (babel:string-to-octets json :encoding :utf-8))
         (header (format nil "Content-Length: ~D~C~C~C~C"
                         (length body-bytes)
                         #\Return #\Newline #\Return #\Newline))
         (header-bytes (babel:string-to-octets header :encoding :utf-8)))
    (concatenate '(vector (unsigned-byte 8)) header-bytes body-bytes)))

(defun parse-lsp-response-from-bytes (bytes start)
  "Parse an LSP response from bytes starting at position START.
   Returns (values parsed-response end-position) or NIL if incomplete."
  (let* ((text (babel:octets-to-string bytes :encoding :utf-8 :start start))
         (header-end-marker (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline))
         (header-end (search header-end-marker text)))
    (when header-end
      (let* ((header-text (subseq text 0 header-end))
             (content-length-match (cl-ppcre:scan-to-strings "Content-Length:\\s*(\\d+)" header-text)))
        (when content-length-match
          (let* ((content-length (parse-integer
                                  (aref (nth-value 1 (cl-ppcre:scan-to-strings
                                                      "Content-Length:\\s*(\\d+)" header-text)) 0)))
                 (body-start (+ header-end 4))
                 (body-text (subseq text body-start (+ body-start content-length))))
            (values (com.inuoe.jzon:parse body-text)
                    (+ start (length (babel:string-to-octets
                                      (subseq text 0 (+ body-start content-length))
                                      :encoding :utf-8))))))))))

;;; Test server using pipes (SBCL-specific)

(defmacro with-test-server (&body body)
  "Execute body with a fresh test server context.
   Provides SEND-REQUEST function for sending requests and getting responses."
  `(multiple-value-bind (c2s-read c2s-write) (sb-posix:pipe)
     (multiple-value-bind (s2c-read s2c-write) (sb-posix:pipe)
       (let ((server-input (sb-sys:make-fd-stream c2s-read
                                                   :input t
                                                   :element-type '(unsigned-byte 8)))
             (server-output (sb-sys:make-fd-stream s2c-write
                                                    :output t
                                                    :element-type '(unsigned-byte 8)))
             (client-input (sb-sys:make-fd-stream s2c-read
                                                   :input t
                                                   :element-type '(unsigned-byte 8)))
             (client-output (sb-sys:make-fd-stream c2s-write
                                                    :output t
                                                    :element-type '(unsigned-byte 8)))
             (server-thread nil))
         (unwind-protect
             (progn
               ;; Reset server state
               (clef-lsp/server:reset)
               ;; Register handlers
               (clef-lsp/server::register-handlers)
               ;; Start server thread
               (setf server-thread
                     (bordeaux-threads:make-thread
                      (lambda ()
                        (handler-case
                            (loop
                              (let ((request (clef-jsonrpc/messages:read-lsp-message server-input)))
                                (when request
                                  (let* ((id (clef-jsonrpc/types:request-id request))
                                         (response (clef-lsp/server::handle-lsp-request id request)))
                                    (when response
                                      (clef-jsonrpc/messages:write-lsp-message response server-output))))))
                          (end-of-file () nil)
                          (error (e)
                            (format *error-output* "Server error: ~A~%" e))))
                      :name "test-lsp-server"))
               ;; Provide send-request function
               (flet ((send-request (method params &key (id 1))
                        "Send an LSP request and get the response"
                        (let* ((request (make-lsp-request method params :id id))
                               (bytes (encode-lsp-message request)))
                          ;; Write request
                          (write-sequence bytes client-output)
                          (force-output client-output)
                          ;; Read response (with timeout protection)
                          (let ((response-bytes (make-array 8192
                                                            :element-type '(unsigned-byte 8)
                                                            :fill-pointer 0)))
                            ;; Read until we have a complete response
                            (loop for byte = (read-byte client-input nil :eof)
                                  until (eq byte :eof)
                                  do (vector-push-extend byte response-bytes)
                                  when (> (length response-bytes) 20)
                                    do (multiple-value-bind (response end-pos)
                                           (ignore-errors (parse-lsp-response-from-bytes response-bytes 0))
                                         (when response
                                           (return response))))))))
                 ,@body))
           ;; Cleanup
           (when server-thread
             (ignore-errors (bordeaux-threads:destroy-thread server-thread)))
           (ignore-errors (close client-output))
           (ignore-errors (close client-input))
           (ignore-errors (close server-output))
           (ignore-errors (close server-input)))))))

;;; Fixture files

(defun test-temp-dir ()
  "Project-local scratch directory for test fixtures.

Global /tmp is deliberately avoided: it is not writable under sandboxed
environments, and keeping scratch inside the project makes cleanup safe and the
checkout self-contained."
  (let ((dir (asdf:system-relative-pathname :clef-lsp "tmp/test/")))
    (ensure-directories-exist dir)
    dir))

(defvar *temp-file-counter* 0
  "Serial number for fixture files. get-universal-time alone has one-second
resolution, so tests running within the same second collided on a single path --
which silently aliased file-a and file-b in the cross-file tests.")

(defun write-temp-file (content)
  "Write content to a fresh temp file and return its path"
  (let ((path (namestring
               (merge-pathnames (format nil "clef-test-~D-~D.lisp"
                                        (get-universal-time)
                                        (incf *temp-file-counter*))
                                (test-temp-dir)))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    path))

(defun delete-temp-file (path)
  "Delete a temp file"
  (ignore-errors (delete-file path)))

;;; Server setup scaffolding
;;;
;;; Lives here rather than in document-tests.lisp because every test file needs
;;; it. INIT-SERVER is a macro, so it is only visible to files loaded after the
;;; one that defines it -- which meant a test file loaded earlier could not use
;;; it, and failed with "is a macro, not a function" rather than anything that
;;; pointed at load order.

(defun make-init-params ()
  "Create params for initialize request"
  (dict "processId" 12345
        "capabilities" (dict)
        "rootUri" "file:///tmp/test-workspace"
        "workspaceFolders" (vector (dict "uri" "file:///tmp/test-workspace"
                                         "name" "test"))))

(defmacro init-server ()
  "Initialize the server within with-direct-handler-test context"
  `(progn
     (call-handler "initialize" (make-init-params))
     (call-handler "initialized" (dict) :id nil)))

;;; Simpler direct handler testing (no pipes, faster)

(defmacro with-direct-handler-test (&body body)
  "Execute body with server state reset but test handlers directly.
   Provides CALL-HANDLER function for invoking handlers.

CALL-HANDLER is an FLET, so it exists only inside this macro's body. A top-level
helper function that calls it compiles fine and fails at run time with \"The
function CLEF-TEST::CALL-HANDLER is undefined\" -- a message pointing nowhere
near the cause. Helpers that need it must be macros, or take the response as an
argument. This has bitten three times."
  `(progn
     ;; Reset server state
     (clef-lsp/server:reset)
     ;; Register handlers
     (clef-lsp/server::register-handlers)
     ;; Provide call-handler function
     (flet ((call-handler (method params &key (id 1))
              "Call an LSP handler directly and get the result"
              (let ((request (make-instance 'clef-jsonrpc/types:jsonrpc-request
                                            :id id
                                            :method method
                                            :params (clef-jsonrpc/messages::make-hash-table-hyphen-case params))))
                (clef-lsp/server::handle-lsp-request id request))))
       ,@body)))

;;; Test runner

;; ANSI color codes
(defparameter *color-green* (format nil "~C[32m" #\Esc))
(defparameter *color-red* (format nil "~C[31m" #\Esc))
(defparameter *color-yellow* (format nil "~C[33m" #\Esc))
(defparameter *color-reset* (format nil "~C[0m" #\Esc))

(defun run-test (name test-fn)
  "Run a single test and return result"
  (handler-case
      (progn
        (funcall test-fn)
        (list :name name :status :pass))
    (test-failure (e)
      (list :name name :status :fail :message (test-failure-message e)))
    (error (e)
      (list :name name :status :error :message (format nil "~A" e)))))

(defun run-all-tests ()
  "Run all registered tests"
  (format t "~%Running CLEF LSP tests...~%")
  (format t "~%")
  (let ((results nil)
        (pass-count 0)
        (fail-count 0)
        (error-count 0)
        (failures nil))
    ;; Sort tests by name for consistent ordering
    (let ((test-names (sort (loop for name being the hash-keys of *tests* collect name) #'string<)))
      (dolist (name test-names)
        (let* ((test-fn (gethash name *tests*))
               (result (run-test name test-fn)))
          (push result results)
          (case (getf result :status)
            (:pass
             (format t "  ~A~A~A ~A~%" *color-green* #\CHECK_MARK *color-reset* name)
             (incf pass-count))
            (:fail
             (format t "  ~A~A ~A~A~%" *color-red* #\BALLOT_X name *color-reset*)
             (push result failures)
             (incf fail-count))
            (:error
             (format t "  ~A! ~A~A~%" *color-yellow* name *color-reset*)
             (push result failures)
             (incf error-count))))))
    ;; Print failures at the end
    (when failures
      (format t "~%~AFailures:~A~%" *color-red* *color-reset*)
      (dolist (f (nreverse failures))
        (format t "  ~A: ~A~%" (getf f :name) (getf f :message))))
    ;; Summary
    (format t "~%~A~%" (make-string 50 :initial-element #\-))
    (if (zerop (+ fail-count error-count))
        (format t "~AResults: ~D passed, ~D failed, ~D errors~A~%"
                *color-green* pass-count fail-count error-count *color-reset*)
        (format t "~AResults: ~D passed, ~D failed, ~D errors~A~%"
                *color-red* pass-count fail-count error-count *color-reset*))
    (setf *test-results* (nreverse results))
    (values (zerop (+ fail-count error-count))
            *test-results*)))
