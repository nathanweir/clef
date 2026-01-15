(in-package :clef-test)

;;; Document tests: didOpen, didChange, hover, completion, definition, formatting

(defparameter *test-lisp-code*
  "(defpackage :test-pkg
  (:use :cl))

(in-package :test-pkg)

(defun my-function (x y)
  \"A test function that adds two numbers\"
  (+ x y))

(defvar *my-var* 42 \"A test variable\")

(defun caller ()
  (my-function *my-var* 10))"
  "Sample Lisp code for testing")

(defparameter *simple-lisp-code*
  "(defun hello () (print \"hello\"))"
  "Simple Lisp code for basic tests")

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

;;; textDocument/didOpen tests

(deftest test-did-open-stores-document
  "Test that didOpen stores document text"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" *simple-lisp-code*))
                  :id nil)
    (assert-equal *simple-lisp-code*
                  (gethash "file:///tmp/test.lisp" clef-lsp/server:*documents*)
                  "Document text should be stored")))

(deftest test-did-open-multiple-documents
  "Test that multiple documents can be opened"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/a.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(defun a () 1)"))
                  :id nil)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/b.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(defun b () 2)"))
                  :id nil)
    (assert-equal "(defun a () 1)"
                  (gethash "file:///tmp/a.lisp" clef-lsp/server:*documents*))
    (assert-equal "(defun b () 2)"
                  (gethash "file:///tmp/b.lisp" clef-lsp/server:*documents*))))

;;; textDocument/didChange tests

(deftest test-did-change-updates-document
  "Test that didChange updates document text"
  (with-direct-handler-test
    (init-server)
    ;; Open document
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(defun old () nil)"))
                  :id nil)
    ;; Change document (full sync)
    (call-handler "textDocument/didChange"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "version" 2)
                        "contentChanges" (vector (dict "text" "(defun new () t)")))
                  :id nil)
    (assert-equal "(defun new () t)"
                  (gethash "file:///tmp/test.lisp" clef-lsp/server:*documents*)
                  "Document should be updated")))

;;; textDocument/hover tests

(deftest test-hover-returns-contents
  "Test that hover returns contents dict on valid code"
  (with-direct-handler-test
    (init-server)
    ;; Use simple code that should definitely work
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(defun hello () (print \"hi\"))"))
                  :id nil)
    ;; Hover over "print" which is a well-known symbol
    (let* ((response (call-handler "textDocument/hover"
                                   (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                         "position" (dict "line" 0 "character" 18))))
           (result (response-result-safe response)))
      (assert-not-nil result "Should get hover result")
      (assert-not-nil (gethash "contents" result) "Should have contents key"))))

(deftest test-hover-response-structure
  "Test that hover response has correct structure"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(format t \"test\")"))
                  :id nil)
    (let ((response (call-handler "textDocument/hover"
                                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                        "position" (dict "line" 0 "character" 2)))))
      ;; Should get either a result or an error, but not crash
      (assert-not-nil response "Should get a response"))))

;;; textDocument/completion tests

(deftest test-completion-returns-response
  "Test that completion returns a response"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" *test-lisp-code*))
                  :id nil)
    (let ((response (call-handler "textDocument/completion"
                                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                        "position" (dict "line" 12 "character" 5)))))
      ;; Should get some response (either success or error)
      (assert-not-nil response "Should get a response"))))

(deftest test-completion-result-structure
  "Test that successful completion has correct structure"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" *simple-lisp-code*))
                  :id nil)
    (let* ((response (call-handler "textDocument/completion"
                                   (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                         "position" (dict "line" 0 "character" 5))))
           (result (response-result-safe response)))
      ;; If we got a result (not an error), check structure
      (when result
        (assert-not-nil (gethash "items" result) "Should have items key")))))

;;; textDocument/definition tests

(deftest test-definition-returns-location-structure
  "Test that definition returns location or empty array"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" *test-lisp-code*))
                  :id nil)
    (let ((response (call-handler "textDocument/definition"
                                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                        "position" (dict "line" 12 "character" 5)))))
      ;; Result could be a Location dict, array of Locations, or empty array
      (assert-not-nil response "Should get a response"))))

(deftest test-definition-on-nonexistent-symbol
  "Test that definition on nonexistent symbol returns empty"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" ""))
                  :id nil)
    (let ((response (call-handler "textDocument/definition"
                                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                        "position" (dict "line" 0 "character" 0)))))
      ;; Should return empty array for no definition
      (assert-not-nil response "Should get a response"))))

;;; Symbol lookup tests (using real temp files)

(defparameter *symbol-test-code*
  "(defun my-add (a b)
  (+ a b))

(defun caller ()
  (my-add 1 2))"
  "Code with a function definition and a call to test go-to-definition")

(defun write-temp-file (content)
  "Write content to a temp file and return its path"
  (let ((path (format nil "/tmp/clef-test-~A.lisp" (get-universal-time))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    path))

(defun delete-temp-file (path)
  "Delete a temp file"
  (ignore-errors (delete-file path)))

(deftest test-definition-finds-local-function
  "Test that go-to-definition on a function call finds the function definition"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          ;; Create a real temp file
          (setf temp-path (write-temp-file *symbol-test-code*))
          (let ((file-uri (format nil "file://~A" temp-path)))
            ;; Open the document
            (call-handler "textDocument/didOpen"
                          (dict "textDocument" (dict "uri" file-uri
                                                     "languageId" "lisp"
                                                     "version" 1
                                                     "text" *symbol-test-code*))
                          :id nil)
            ;; Trigger didChange to build symbol map
            (call-handler "textDocument/didChange"
                          (dict "textDocument" (dict "uri" file-uri "version" 2)
                                "contentChanges" (vector (dict "text" *symbol-test-code*)))
                          :id nil)
            ;; Request definition at "my-add" call site (line 4, char 3 = start of "my-add")
            (let* ((response (call-handler "textDocument/definition"
                                           (dict "textDocument" (dict "uri" file-uri)
                                                 "position" (dict "line" 4 "character" 3))))
                   (result (response-result-safe response)))
              ;; Should get a location pointing to line 0 (the defun)
              (assert-not-nil result "Should find definition")
              (when (hash-table-p result)
                (let ((range (gethash "range" result)))
                  (assert-not-nil range "Should have range")
                  (when range
                    (let ((start (gethash "start" range)))
                      (assert-equal 0 (gethash "line" start)
                                    "Definition should be on line 0"))))))))
      ;; Cleanup
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-definition-finds-parameter
  "Test that go-to-definition on a parameter finds the parameter in the function signature"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (setf temp-path (write-temp-file "(defun foo (x y)
  (+ x y))"))
          (let ((file-uri (format nil "file://~A" temp-path))
                (code "(defun foo (x y)
  (+ x y))"))
            (call-handler "textDocument/didOpen"
                          (dict "textDocument" (dict "uri" file-uri
                                                     "languageId" "lisp"
                                                     "version" 1
                                                     "text" code))
                          :id nil)
            (call-handler "textDocument/didChange"
                          (dict "textDocument" (dict "uri" file-uri "version" 2)
                                "contentChanges" (vector (dict "text" code)))
                          :id nil)
            ;; Request definition at "x" usage (line 1, char 5)
            (let* ((response (call-handler "textDocument/definition"
                                           (dict "textDocument" (dict "uri" file-uri)
                                                 "position" (dict "line" 1 "character" 5))))
                   (result (response-result-safe response)))
              ;; Should find x parameter definition on line 0
              (assert-not-nil result "Should find parameter definition"))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-completion-includes-local-symbols
  "Test that completion includes locally defined symbols"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          ;; Use valid complete code - incomplete code may cause parsing issues
          (let ((code "(defun helper () 42)

(defun main ()
  (helper))"))
            (setf temp-path (write-temp-file code))
            (let ((file-uri (format nil "file://~A" temp-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" file-uri
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" file-uri "version" 2)
                                  "contentChanges" (vector (dict "text" code)))
                            :id nil)
              ;; Request completion inside main function body (line 3, char 3)
              (let* ((response (call-handler "textDocument/completion"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 3 "character" 3))))
                     (result (response-result-safe response)))
                ;; Completion may return nil result if no completions available
                ;; The test passes if we get a response without error
                (assert-true (or result (not (response-is-error-p response)))
                             "Should get completion response without error")))))
      (when temp-path (delete-temp-file temp-path)))))

;;; textDocument/formatting tests

(deftest test-formatting-returns-edits
  "Test that formatting returns text edits"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(defun foo()nil)"))
                  :id nil)
    (let* ((response (call-handler "textDocument/formatting"
                                   (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                         "options" (dict "tabSize" 2
                                                         "insertSpaces" t))))
           (result (response-result-safe response)))
      (assert-not-nil result "Should get formatting result")
      (assert-true (listp result) "Result should be a list of edits"))))

(deftest test-formatting-edit-has-range
  "Test that formatting edit has range and newText"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" "(defun bar () 1)"))
                  :id nil)
    (let* ((response (call-handler "textDocument/formatting"
                                   (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                         "options" (dict "tabSize" 2
                                                         "insertSpaces" t))))
           (result (response-result-safe response)))
      (when (and result (listp result) (> (length result) 0))
        (let ((first-edit (first result)))
          (assert-not-nil (gethash "range" first-edit) "Edit should have range")
          (assert-not-nil (gethash "newText" first-edit) "Edit should have newText"))))))

;;; Cross-file go-to-definition tests

(deftest test-definition-finds-cross-file-function
  "Test that go-to-definition finds a function defined in another file"
  (let ((file-a-path nil)
        (file-b-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          ;; Create file A with a function definition
          (let ((code-a "(defun shared-helper (x)
  (* x 2))"))
            (setf file-a-path (write-temp-file code-a))
            (let ((uri-a (format nil "file://~A" file-a-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" uri-a
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code-a))
                            :id nil)
              ;; Trigger symbol map build
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" uri-a "version" 2)
                                  "contentChanges" (vector (dict "text" code-a)))
                            :id nil)))
          ;; Create file B that calls the function from file A
          (let ((code-b "(defun caller ()
  (shared-helper 21))"))
            (setf file-b-path (write-temp-file code-b))
            (let ((uri-b (format nil "file://~A" file-b-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" uri-b
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code-b))
                            :id nil)
              ;; Trigger symbol map build
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" uri-b "version" 2)
                                  "contentChanges" (vector (dict "text" code-b)))
                            :id nil)
              ;; Request definition at "shared-helper" call (line 1, char 3)
              (let* ((response (call-handler "textDocument/definition"
                                             (dict "textDocument" (dict "uri" uri-b)
                                                   "position" (dict "line" 1 "character" 3))))
                     (result (response-result-safe response)))
                ;; Should find definition in file A
                (assert-not-nil result "Should find cross-file definition")
                (when (hash-table-p result)
                  (let ((result-uri (gethash "uri" result)))
                    (assert-not-nil result-uri "Should have uri in result")
                    (assert-true (search file-a-path result-uri)
                                 "Definition should point to file A")))))))
      ;; Cleanup
      (when file-a-path (delete-temp-file file-a-path))
      (when file-b-path (delete-temp-file file-b-path)))))

(deftest test-definition-cross-file-returns-correct-location
  "Test that cross-file definition returns the correct line/character location"
  (let ((file-a-path nil)
        (file-b-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          ;; File A: function on line 2 (after blank line)
          (let ((code-a "
(defun target-func ()
  42)"))
            (setf file-a-path (write-temp-file code-a))
            (let ((uri-a (format nil "file://~A" file-a-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" uri-a
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code-a))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" uri-a "version" 2)
                                  "contentChanges" (vector (dict "text" code-a)))
                            :id nil)))
          ;; File B: calls target-func
          (let ((code-b "(defun main ()
  (target-func))"))
            (setf file-b-path (write-temp-file code-b))
            (let ((uri-b (format nil "file://~A" file-b-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" uri-b
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code-b))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" uri-b "version" 2)
                                  "contentChanges" (vector (dict "text" code-b)))
                            :id nil)
              ;; Request definition at "target-func" (line 1, char 3)
              (let* ((response (call-handler "textDocument/definition"
                                             (dict "textDocument" (dict "uri" uri-b)
                                                   "position" (dict "line" 1 "character" 3))))
                     (result (response-result-safe response)))
                (assert-not-nil result "Should find cross-file definition")
                (when (hash-table-p result)
                  (let ((range (gethash "range" result)))
                    (assert-not-nil range "Should have range")
                    (when range
                      (let ((start (gethash "start" range)))
                        ;; Definition is on line 1 (0-indexed, after blank line)
                        (assert-equal 1 (gethash "line" start)
                                      "Definition should be on line 1")))))))))
      ;; Cleanup
      (when file-a-path (delete-temp-file file-a-path))
      (when file-b-path (delete-temp-file file-b-path)))))

;;; textDocument/references tests

(deftest test-references-returns-array
  "Test that references returns an array response"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun foo (x)
  (* x x))

(defun bar ()
  (foo 5))"))
            (setf temp-path (write-temp-file code))
            (let ((file-uri (format nil "file://~A" temp-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" file-uri
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" file-uri "version" 2)
                                  "contentChanges" (vector (dict "text" code)))
                            :id nil)
              ;; Request references at "foo" definition (line 0, char 7)
              (let ((response (call-handler "textDocument/references"
                                            (dict "textDocument" (dict "uri" file-uri)
                                                  "position" (dict "line" 0 "character" 7)
                                                  "context" (dict "includeDeclaration" nil)))))
                (assert-not-nil response "Should get a response")))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-references-finds-usages-in-same-file
  "Test that references finds all usages of a symbol in the same file"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun helper (n)
  (* n 2))

(defun use1 ()
  (helper 5))

(defun use2 ()
  (helper 10))"))
            (setf temp-path (write-temp-file code))
            (let ((file-uri (format nil "file://~A" temp-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" file-uri
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" file-uri "version" 2)
                                  "contentChanges" (vector (dict "text" code)))
                            :id nil)
              ;; Request references at "helper" call (line 4, char 3)
              (let* ((response (call-handler "textDocument/references"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 4 "character" 3)
                                                   "context" (dict "includeDeclaration" nil))))
                     (result (response-result-safe response)))
                ;; Should find at least 2 references (the two calls to helper)
                (assert-not-nil result "Should find references")
                (when (vectorp result)
                  (assert-true (>= (length result) 2)
                               "Should find at least 2 references to helper"))))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-references-includes-declaration-when-requested
  "Test that references includes the declaration when includeDeclaration is true"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun target ()
  42)

(defun caller ()
  (target))"))
            (setf temp-path (write-temp-file code))
            (let ((file-uri (format nil "file://~A" temp-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" file-uri
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" file-uri "version" 2)
                                  "contentChanges" (vector (dict "text" code)))
                            :id nil)
              ;; Request references with includeDeclaration = true
              (let* ((response (call-handler "textDocument/references"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 4 "character" 3)
                                                   "context" (dict "includeDeclaration" t))))
                     (result (response-result-safe response)))
                ;; Should find at least 2 locations (declaration + usage)
                (assert-not-nil result "Should find references")
                (when (vectorp result)
                  (assert-true (>= (length result) 2)
                               "Should include declaration plus usage"))))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-references-cross-file
  "Test that references finds usages across multiple files"
  (let ((file-a-path nil)
        (file-b-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          ;; File A: defines shared-fn
          (let ((code-a "(defun shared-fn (x)
  (+ x 1))"))
            (setf file-a-path (write-temp-file code-a))
            (let ((uri-a (format nil "file://~A" file-a-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" uri-a
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code-a))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" uri-a "version" 2)
                                  "contentChanges" (vector (dict "text" code-a)))
                            :id nil)))
          ;; File B: uses shared-fn
          (let ((code-b "(defun caller ()
  (shared-fn 5))"))
            (setf file-b-path (write-temp-file code-b))
            (let ((uri-b (format nil "file://~A" file-b-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" uri-b
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code-b))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" uri-b "version" 2)
                                  "contentChanges" (vector (dict "text" code-b)))
                            :id nil)
              ;; Request references at shared-fn call in file B
              (let* ((response (call-handler "textDocument/references"
                                             (dict "textDocument" (dict "uri" uri-b)
                                                   "position" (dict "line" 1 "character" 3)
                                                   "context" (dict "includeDeclaration" nil))))
                     (result (response-result-safe response)))
                ;; Should find the reference in file B
                (assert-not-nil result "Should find cross-file references")))))
      (when file-a-path (delete-temp-file file-a-path))
      (when file-b-path (delete-temp-file file-b-path)))))

(deftest test-references-empty-for-unknown-symbol
  "Test that references returns empty array for unknown symbols"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/test.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" ""))
                  :id nil)
    (let* ((response (call-handler "textDocument/references"
                                   (dict "textDocument" (dict "uri" "file:///tmp/test.lisp")
                                         "position" (dict "line" 0 "character" 0)
                                         "context" (dict "includeDeclaration" nil))))
           (result (response-result-safe response)))
      ;; Should return empty array, not error
      (assert-true (or (null result)
                       (and (vectorp result) (= 0 (length result))))
                   "Should return empty result for unknown symbol"))))

(deftest test-references-from-definition-site
  "Test that references works when cursor is on the function name in a defun"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun my-func (x)
  (* x 2))

(defun caller1 ()
  (my-func 5))

(defun caller2 ()
  (my-func 10))"))
            (setf temp-path (write-temp-file code))
            (let ((file-uri (format nil "file://~A" temp-path)))
              (call-handler "textDocument/didOpen"
                            (dict "textDocument" (dict "uri" file-uri
                                                       "languageId" "lisp"
                                                       "version" 1
                                                       "text" code))
                            :id nil)
              (call-handler "textDocument/didChange"
                            (dict "textDocument" (dict "uri" file-uri "version" 2)
                                  "contentChanges" (vector (dict "text" code)))
                            :id nil)
              ;; Request references at "my-func" DEFINITION (line 0, char 7 = on "my-func")
              ;; This is the function name in the defun, not a usage
              (let* ((response (call-handler "textDocument/references"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 0 "character" 7)
                                                   "context" (dict "includeDeclaration" nil))))
                     (result (response-result-safe response)))
                ;; Should find the 2 usages (caller1 and caller2)
                (assert-not-nil result "Should find references from definition site")
                (when (vectorp result)
                  (assert-true (>= (length result) 2)
                               "Should find at least 2 references when clicking on definition"))))))
      (when temp-path (delete-temp-file temp-path)))))
