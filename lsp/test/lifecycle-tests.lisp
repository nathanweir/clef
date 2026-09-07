(defpackage :clef-lsp/test/lifecycle-tests
  (:use :cl)
  (:import-from :clef-lsp/test/framework #:assert-equal #:assert-nil #:assert-not-nil
                #:assert-true #:call-handler #:deftest
                #:response-result-safe #:with-direct-handler-test)
  (:import-from :serapeum #:dict #:href)
  (:local-nicknames
    (:ctx :clef-lsp/src/context)
    (:rpc :clef-lsp/src/jsonrpc/types)))

(in-package :clef-lsp/test/lifecycle-tests)

;;; Lifecycle tests: initialize, initialized, shutdown

(defun make-minimal-initialize-params ()
  "Create minimal initialize params for testing"
  (dict "processId" 12345
        "capabilities" (dict)
        "rootUri" "file:///tmp/test-workspace"
        "workspaceFolders" (vector (dict "uri" "file:///tmp/test-workspace"
                                         "name" "test"))))

(deftest test-initialize-returns-capabilities
  "Test that initialize returns server capabilities"
  (with-direct-handler-test
    (let* ((response (call-handler "initialize" (make-minimal-initialize-params))))
      (assert-not-nil response "Expected a response from initialize")
      (let ((result (response-result-safe response)))
        (assert-not-nil result "Expected result in response")
        (assert-not-nil (href result "capabilities") "Expected capabilities in result")))))

(deftest test-initialize-sets-workspace-root
  "Test that initialize sets the workspace root"
  (with-direct-handler-test
    (call-handler "initialize" (make-minimal-initialize-params))
    (assert-equal "file:///tmp/test-workspace"
                  ctx:workspace-root
                  "Workspace root should be set")))

(deftest test-initialize-stores-client-capabilities
  "Test that initialize stores client capabilities"
  (with-direct-handler-test
    (let ((params (dict "processId" 12345
                        "capabilities" (dict "textDocument" (dict "hover" t))
                        "rootUri" "file:///tmp/test"
                        "workspaceFolders" (vector (dict "uri" "file:///tmp/test"
                                                         "name" "test")))))
      (call-handler "initialize" params)
      (assert-not-nil ctx:client-capabilities
                      "Client capabilities should be stored"))))

(deftest test-initialized-sets-flag
  "Test that initialized notification sets the initialized flag"
  (with-direct-handler-test
    ;; First initialize
    (call-handler "initialize" (make-minimal-initialize-params))
    ;; Then send initialized notification
    (call-handler "initialized" (dict) :id nil)
    (assert-true ctx:initialized
                 "Server should be marked as initialized")))

(deftest test-server-not-initialized-error
  "Test that requests before initialized return ServerNotInitialized error"
  (with-direct-handler-test
    ;; Don't call initialize first
    (let ((response (call-handler "textDocument/hover"
                                  (dict "textDocument" (dict "uri" "file:///test.lisp")
                                        "position" (dict "line" 0 "character" 0)))))
      (assert-not-nil response)
      ;; Should be an error response
      (assert-true (typep response 'rpc:jsonrpc-error-response)
                   "Expected error response for uninitialized server"))))

(deftest test-shutdown-resets-state
  "Test that shutdown resets server state"
  (with-direct-handler-test
    ;; Initialize the server
    (call-handler "initialize" (make-minimal-initialize-params))
    (call-handler "initialized" (dict) :id nil)
    ;; Verify initialized
    (assert-true ctx:initialized)
    ;; Shutdown
    (call-handler "shutdown" (dict))
    ;; State should be reset
    (assert-nil ctx:initialized
                "Server should not be initialized after shutdown")))

(deftest test-capabilities-include-expected-providers
  "Test that server capabilities include expected providers"
  (with-direct-handler-test
    (let* ((response (call-handler "initialize" (make-minimal-initialize-params)))
           (result (response-result-safe response))
           (capabilities (href result "capabilities")))
      (assert-not-nil (href capabilities "hoverProvider")
                      "Should have hover provider")
      (assert-not-nil (href capabilities "definitionProvider")
                      "Should have definition provider")
      (assert-not-nil (href capabilities "completionProvider")
                      "Should have completion provider")
      (assert-not-nil (href capabilities "documentFormattingProvider")
                      "Should have formatting provider"))))
