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
;;; Reference scoping
;;; ---------------------------------------------------------------------------

(defun reference-positions (response)
  "Sorted (line . character) pairs from a textDocument/references response.

Takes the response rather than a position because CALL-HANDLER is an FLET bound
by WITH-DIRECT-HANDLER-TEST and is not visible to a top-level function."
  (let ((result (response-result-safe response)))
    (when (vectorp result)
      (sort (map 'list (lambda (loc)
                         (let ((s (gethash "start" (gethash "range" loc))))
                           (cons (gethash "line" s) (gethash "character" s))))
                 result)
            (lambda (a b) (or (< (car a) (car b))
                              (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))))

(defun references-params (uri line character)
  (dict "textDocument" (dict "uri" uri)
        "position" (dict "line" line "character" character)
        "context" (dict "includeDeclaration" t)))

(deftest test-references-to-a-let-binding-stay-in-scope
  "References to a LET-bound variable must not include unrelated same-named symbols"
  (let ((temp-path nil))
    (unwind-protect
         (with-direct-handler-test
           (init-server)
           ;; TOTAL appears as a defclass slot, as a LET binding, and as two uses
           ;; of that binding. Only the binding and its uses are references to it.
           (let* ((code "(defclass boxed ()
  ((total :initarg :total :accessor boxed-total)))

(defun compute (n)
  (let ((total (* n 2)))
    (list total total)))")
                  (path (write-temp-file code))
                  (uri (format nil "file://~A" path)))
             (setf temp-path path)
             (call-handler "textDocument/didOpen"
                           (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                      "version" 1 "text" code))
                           :id nil)
             (let ((lines (mapcar #'car
                                  (reference-positions
                                   (call-handler "textDocument/references"
                                                 (references-params uri 4 11))))))
               (assert-not-nil lines "Should find references to the LET binding")
               ;; Line 1 is the defclass slot. It shares only a name.
               (assert-nil (member 1 lines)
                           "Must not report the defclass slot of the same name")
               ;; Every result must be inside the DEFUN, which starts at line 3.
               (assert-true (every (lambda (l) (>= l 3)) lines)
                            "Every reference must fall inside the binding's scope"))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-references-are-not-duplicated
  "The declaration must not be reported twice when includeDeclaration is set"
  (let ((temp-path nil))
    (unwind-protect
         (with-direct-handler-test
           (init-server)
           (let* ((code "(defun holder ()
  (let ((item 1))
    item))")
                  (path (write-temp-file code))
                  (uri (format nil "file://~A" path)))
             (setf temp-path path)
             (call-handler "textDocument/didOpen"
                           (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                      "version" 1 "text" code))
                           :id nil)
             (let ((refs (reference-positions
                          (call-handler "textDocument/references"
                                        (references-params uri 1 9)))))
               (assert-not-nil refs "Should find references")
               (assert-equal (length refs) (length (remove-duplicates refs :test #'equal))
                             "No location should be reported more than once"))))
      (when temp-path (delete-temp-file temp-path)))))

;;; ---------------------------------------------------------------------------
;;; documentSymbol
;;; ---------------------------------------------------------------------------

(deftest test-document-symbol-lists-top-level-definitions
  "documentSymbol must report the file's top-level definitions"
  (let ((temp-path nil))
    (unwind-protect
         (with-direct-handler-test
           (init-server)
           (let* ((code "(defvar *tally* 0)

(defun add-one (n)
  (let ((local 1))
    (+ n local)))

(defmacro twice (form)
  `(progn ,form ,form))")
                  (path (write-temp-file code))
                  (uri (format nil "file://~A" path)))
             (setf temp-path path)
             (call-handler "textDocument/didOpen"
                           (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                      "version" 1 "text" code))
                           :id nil)
             (let* ((response (call-handler "textDocument/documentSymbol"
                                            (dict "textDocument" (dict "uri" uri))))
                    (result (response-result-safe response))
                    (names (when (vectorp result)
                             (map 'list (lambda (s) (gethash "name" s)) result))))
               (assert-true (answered-p response) "documentSymbol must answer")
               (assert-not-nil names "Should report symbols")
               (assert-true (member "*tally*" names :test #'string=) "Should list the defvar")
               (assert-true (member "add-one" names :test #'string=) "Should list the defun")
               (assert-true (member "twice" names :test #'string=) "Should list the defmacro")
               ;; An outline listing every local binding would be unreadable, and
               ;; no editor presents them that way.
               (assert-nil (member "local" names :test #'string=)
                           "Should NOT list LET-bound locals"))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-document-symbol-range-contains-selection-range
  "The spec requires selectionRange to be contained within range"
  (let ((temp-path nil))
    (unwind-protect
         (with-direct-handler-test
           (init-server)
           (let* ((code "(defun spans-several-lines (a b)
  (list a
        b))")
                  (path (write-temp-file code))
                  (uri (format nil "file://~A" path)))
             (setf temp-path path)
             (call-handler "textDocument/didOpen"
                           (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                      "version" 1 "text" code))
                           :id nil)
             (let* ((response (call-handler "textDocument/documentSymbol"
                                            (dict "textDocument" (dict "uri" uri))))
                    (result (response-result-safe response))
                    (sym (when (and (vectorp result) (plusp (length result)))
                           (aref result 0))))
               (assert-not-nil sym "Should report the function")
               (when sym
                 (let ((r-start (gethash "line" (gethash "start" (gethash "range" sym))))
                       (r-end (gethash "line" (gethash "end" (gethash "range" sym))))
                       (s-start (gethash "line" (gethash "start" (gethash "selectionRange" sym)))))
                   (assert-true (and (<= r-start s-start) (<= s-start r-end))
                                "selectionRange must fall inside range")
                   ;; And range must span the whole definition, not just the
                   ;; name -- that is what gives an editor its breadcrumb.
                   (assert-true (> r-end r-start)
                                "range should cover the whole multi-line definition"))))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-document-symbol-on-unknown-file-answers-empty
  "documentSymbol for a file with no symbol map must answer, with an empty array"
  (with-direct-handler-test
    (init-server)
    (let ((response (call-handler "textDocument/documentSymbol"
                                  (dict "textDocument"
                                        (dict "uri" "file:///tmp/never-opened.lisp")))))
      (assert-true (answered-p response) "Must still answer")
      (assert-true (vectorp (response-result-safe response))
                   "Empty is an array, not null -- the file simply has no symbols"))))

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
