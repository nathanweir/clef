(in-package :clef-test)

;;; JSON-RPC protocol contract tests.
;;;
;;; These assert on the *shape of the exchange* rather than on any handler's
;;; answer: who gets replied to, and what a reply looks like when there is
;;; nothing to say. That distinction had no coverage at all, which is how the
;;; server came to leave requests unanswered without a single test failing.
;;;
;;; See docs/surveys/lsp-review.md §1.1.

;;; ---------------------------------------------------------------------------
;;; Requests are always answered
;;; ---------------------------------------------------------------------------

(deftest test-request-finding-nothing-is-still-answered
  "A request whose handler has no result must get a reply with a null result"
  (with-direct-handler-test
    (init-server)
    ;; A file containing only a comment: signatureHelp has genuinely nothing to
    ;; say. That is a null result, NOT silence -- the client is holding an id
    ;; open and waiting for it.
    (let ((code ";;; just a comment"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/proto-sig.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let ((response (call-handler "textDocument/signatureHelp"
                                    (dict "textDocument" (dict "uri" "file:///tmp/proto-sig.lisp")
                                          "position" (dict "line" 0 "character" 5))
                                    :id 42)))
        (assert-true (answered-p response)
                     "Server must answer a request even when it has no result")
        (assert-true (response-is-success-p response)
                     "Having no result is success, not an error")
        (assert-nil (response-result-safe response)
                    "And the result itself should be null")
        (assert-equal 42 (clef-jsonrpc/types:response-id response)
                      "The reply must carry the request's id")))))

(deftest test-definition-that-resolves-nothing-is-still-answered
  "Go-to-definition on nothing in particular must still reply"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo () 1)"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/proto-def.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      ;; Position 0 is the opening paren -- not a symbol.
      (let ((response (call-handler "textDocument/definition"
                                    (dict "textDocument" (dict "uri" "file:///tmp/proto-def.lisp")
                                          "position" (dict "line" 0 "character" 0))
                                    :id 7)))
        (assert-true (answered-p response)
                     "Server must answer even when the definition is not found")
        (assert-equal 7 (clef-jsonrpc/types:response-id response)
                      "The reply must carry the request's id")))))

;;; ---------------------------------------------------------------------------
;;; Notifications are never answered
;;; ---------------------------------------------------------------------------

(deftest test-notification-gets-no-reply
  "A notification carries no id and must not be replied to"
  (with-direct-handler-test
    (init-server)
    (let ((response (call-handler "textDocument/didOpen"
                                  (dict "textDocument" (dict "uri" "file:///tmp/proto-notif.lisp"
                                                             "languageId" "lisp"
                                                             "version" 1
                                                             "text" "(defun a () 1)"))
                                  :id nil)))
      (assert-nil response "A notification must not be answered"))))

(deftest test-unknown-notification-gets-no-reply
  "An unknown method sent as a notification must not produce an error reply"
  (with-direct-handler-test
    (init-server)
    ;; Previously this produced an error response carrying a null id, which is
    ;; itself a protocol violation -- there is no id to answer.
    (let ((response (call-handler "textDocument/somethingWeDoNotHandle"
                                  (dict "textDocument" (dict "uri" "file:///tmp/x.lisp"))
                                  :id nil)))
      (assert-nil response "An unknown notification must be dropped silently"))))

;;; ---------------------------------------------------------------------------
;;; Document lifecycle
;;; ---------------------------------------------------------------------------

(deftest test-did-open-alone-builds-the-symbol-map
  "didOpen must index the file, without needing a didChange first"
  (let ((temp-path nil))
    (unwind-protect
         (with-direct-handler-test
           (init-server)
           (let* ((code "(defun open-indexed-fn (x) (+ x 1))
(defun open-caller () (open-indexed-fn 2))")
                  (path (write-temp-file code))
                  (uri (format nil "file://~A" path)))
             (setf temp-path path)
             ;; Deliberately NO didChange. Every other test in the suite sends
             ;; one straight after didOpen, which is exactly what hid the fact
             ;; that didOpen did not index at all.
             (call-handler "textDocument/didOpen"
                           (dict "textDocument" (dict "uri" uri
                                                      "languageId" "lisp"
                                                      "version" 1
                                                      "text" code))
                           :id nil)
             ;; Line 1 char 22 is inside the call to OPEN-INDEXED-FN.
             (let* ((response (call-handler "textDocument/definition"
                                            (dict "textDocument" (dict "uri" uri)
                                                  "position" (dict "line" 1 "character" 22))))
                    (result (response-result-safe response)))
               (assert-not-nil result
                               "didOpen alone should be enough to resolve a definition"))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-did-close-drops-the-document
  "didClose must evict the document text"
  (with-direct-handler-test
    (init-server)
    (let ((uri "file:///tmp/proto-close.lisp"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" uri
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" "(defun closing () 1)"))
                    :id nil)
      (assert-not-nil (gethash uri clef-context:documents)
                      "Document should be present after didOpen")
      (let ((response (call-handler "textDocument/didClose"
                                    (dict "textDocument" (dict "uri" uri))
                                    :id nil)))
        (assert-nil response "didClose is a notification and must not be answered"))
      (assert-nil (gethash uri clef-context:documents)
                  "Document should be gone after didClose"))))

;;; ---------------------------------------------------------------------------
;;; Unknown requests fail properly
;;; ---------------------------------------------------------------------------

(deftest test-unknown-request-gets-method-not-found
  "An unknown method sent as a request must get a MethodNotFound error"
  (with-direct-handler-test
    (init-server)
    (let ((response (call-handler "textDocument/somethingWeDoNotHandle"
                                  (dict "textDocument" (dict "uri" "file:///tmp/x.lisp"))
                                  :id 99)))
      (assert-true (answered-p response) "An unknown request must still be answered")
      (assert-true (response-is-error-p response) "And the answer must be an error")
      (assert-equal clef-jsonrpc/types:+method-not-found+
                    (clef-jsonrpc/types:error-code
                     (clef-jsonrpc/types:response-error response))
                    "Error code should be MethodNotFound")
      (assert-equal 99 (clef-jsonrpc/types:response-id response)
                    "The error reply must carry the request's id"))))
