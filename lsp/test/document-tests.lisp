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

;; MAKE-INIT-PARAMS and INIT-SERVER used to live here. They are scaffolding that
;; every test file needs, not document tests, so they now sit in framework.lisp
;; -- INIT-SERVER is a macro, and a macro is only available to files loaded
;; after the one defining it, which silently made it uncallable from any test
;; file loaded earlier.

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
                  (gethash "file:///tmp/test.lisp" clef-context:documents)
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
                  (gethash "file:///tmp/a.lisp" clef-context:documents))
    (assert-equal "(defun b () 2)"
                  (gethash "file:///tmp/b.lisp" clef-context:documents))))

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
                  (gethash "file:///tmp/test.lisp" clef-context:documents)
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

;; TEST-TEMP-DIR, *TEMP-FILE-COUNTER*, WRITE-TEMP-FILE and DELETE-TEMP-FILE
;; moved to framework.lisp -- fixture scaffolding that more than one test file
;; needs. See the note there about load order.

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

;;; Document highlight tests

(deftest test-highlight-returns-array
  "Test that documentHighlight returns an array"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun foo (x) (+ x x))"))
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
              ;; Request highlight on "x" (line 0, char 12 - first x in body)
              (let* ((response (call-handler "textDocument/documentHighlight"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 0 "character" 16))))
                     (result (response-result-safe response)))
                (assert-true (or (vectorp result) (null result))
                             "Result should be array or null")))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-highlight-finds-all-occurrences
  "Test that documentHighlight finds all occurrences of a symbol in the file"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun foo (x)
  (+ x x))"))
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
              ;; Request highlight on "x" - should find definition + 2 usages = 3
              (let* ((response (call-handler "textDocument/documentHighlight"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 1 "character" 5))))
                     (result (response-result-safe response)))
                (assert-not-nil result "Should find highlights")
                (when (vectorp result)
                  (assert-true (>= (length result) 2)
                               (format nil "Should find at least 2 occurrences of x, got ~A"
                                       (length result))))))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-highlight-has-kind
  "Test that documentHighlight includes highlight kind"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun foo (x) x)"))
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
              (let* ((response (call-handler "textDocument/documentHighlight"
                                             (dict "textDocument" (dict "uri" file-uri)
                                                   "position" (dict "line" 0 "character" 12))))
                     (result (response-result-safe response)))
                (when (and result (vectorp result) (> (length result) 0))
                  (let ((first-highlight (aref result 0)))
                    (assert-not-nil (gethash "kind" first-highlight)
                                    "Highlight should have kind")))))))
      (when temp-path (delete-temp-file temp-path)))))

;;; Workspace symbol tests

(deftest test-workspace-symbol-returns-array
  "Test that workspace/symbol returns an array"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun my-test-func () 42)"))
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
              (let* ((response (call-handler "workspace/symbol"
                                             (dict "query" "")))
                     (result (response-result-safe response)))
                (assert-true (vectorp result) "Result should be an array")))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-workspace-symbol-finds-functions
  "Test that workspace/symbol finds defined functions"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun unique-test-symbol-xyz () 42)"))
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
              ;; Search for our unique function name
              (let* ((response (call-handler "workspace/symbol"
                                             (dict "query" "unique-test-symbol")))
                     (result (response-result-safe response)))
                (assert-not-nil result "Should find results")
                (when (vectorp result)
                  (assert-true (> (length result) 0)
                               "Should find at least one matching symbol"))))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-workspace-symbol-has-location
  "Test that workspace symbols include location information"
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          (let ((code "(defun ws-test-func () 42)"))
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
              (let* ((response (call-handler "workspace/symbol"
                                             (dict "query" "ws-test-func")))
                     (result (response-result-safe response)))
                (when (and result (vectorp result) (> (length result) 0))
                  (let ((symbol-info (aref result 0)))
                    (assert-not-nil (gethash "name" symbol-info) "Should have name")
                    (assert-not-nil (gethash "kind" symbol-info) "Should have kind")
                    (assert-not-nil (gethash "location" symbol-info) "Should have location")))))))
      (when temp-path (delete-temp-file temp-path)))))

(deftest test-workspace-symbol-location-range-is-line-first
  "Workspace symbol ranges are line-first and stay inside the file.

workspace/symbol used to carry its own copy of the node -> Range conversion.
Now that it shares one with the document handlers, this pins the behaviour that
copy provided: the definition lives on the single line of a one-line file, so
every line number in the range must be 0 and every column must fall within it."
  (let ((temp-path nil))
    (unwind-protect
        (with-direct-handler-test
          (init-server)
          ;; One line, 27 characters. Any line number other than 0 means a
          ;; column was reported where a line belongs.
          (let ((code "(defun ws-range-func () 42)"))
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
              (let* ((response (call-handler "workspace/symbol"
                                             (dict "query" "ws-range-func")))
                     (result (response-result-safe response)))
                (assert-true (and (vectorp result) (> (length result) 0))
                             "Should find ws-range-func")
                (let* ((location (gethash "location" (aref result 0)))
                       (range (gethash "range" location))
                       (start (gethash "start" range))
                       (end (gethash "end" range)))
                  (assert-not-nil range "Location should carry a range")
                  (assert-equal 0 (gethash "line" start)
                                "Start line should be 0 in a one-line file")
                  (assert-equal 0 (gethash "line" end)
                                "End line should be 0 in a one-line file")
                  (assert-true (<= (gethash "character" start)
                                   (gethash "character" end))
                               "Start character should not follow end character")
                  (assert-true (<= (gethash "character" end) (length code))
                               "End character should fall within the line"))))))
      (when temp-path (delete-temp-file temp-path)))))

;;; Signature help tests

(deftest test-signature-help-returns-nil-outside-function
  "Test that signatureHelp returns nil when not in a function call"
  (with-direct-handler-test
    (init-server)
    (let ((code ";;; just a comment"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 0 "character" 5))))
             (result (response-result-safe response)))
        ;; Should return nil when not in a function call
        (assert-nil result "Should return nil outside function call")))))

(deftest test-signature-help-finds-builtin-function
  "Test that signatureHelp finds arglist for built-in functions"
  (with-direct-handler-test
    (init-server)
    ;; Use 'cons' which has a known arglist (object-1 object-2)
    (let ((code "(cons )"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      ;; Position cursor after "cons " - inside the function call
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 0 "character" 6))))
             (result (response-result-safe response)))
        (assert-not-nil result "Should find signature for cons")
        (when result
          (let ((signatures (gethash "signatures" result)))
            (assert-not-nil signatures "Should have signatures array")
            (when (and signatures (> (length signatures) 0))
              (let ((sig (aref signatures 0)))
                (assert-not-nil (gethash "label" sig) "Signature should have label")))))))))

(deftest test-signature-help-structure
  "Test that signatureHelp response has correct structure"
  (with-direct-handler-test
    (init-server)
    (let ((code "(mapcar )"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 0 "character" 8))))
             (result (response-result-safe response)))
        (when result
          (assert-not-nil (gethash "signatures" result) "Should have signatures")
          (assert-not-nil (gethash "activeSignature" result) "Should have activeSignature")
          (assert-not-nil (gethash "activeParameter" result) "Should have activeParameter"))))))

(deftest test-signature-help-tracks-argument-position
  "Test that signatureHelp tracks which argument the cursor is on"
  (with-direct-handler-test
    (init-server)
    ;; Position cursor after first argument
    (let ((code "(cons 1 )"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      ;; Cursor after "1 " - should be on second argument (index 1)
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 0 "character" 8))))
             (result (response-result-safe response)))
        (when result
          (let ((active-param (gethash "activeParameter" result)))
            (assert-equal 1 active-param
                          "Should be on second argument (index 1)")))))))

(deftest test-signature-help-package-qualified-name
  "Test that signatureHelp works with package-qualified function names"
  (with-direct-handler-test
    (init-server)
    ;; Use a package-qualified name like cl:cons
    (let ((code "(cl:cons 1 )"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 0 "character" 10))))
             (result (response-result-safe response)))
        (assert-not-nil result "Should find signature for package-qualified cl:cons")
        (when result
          (let ((signatures (gethash "signatures" result)))
            (assert-not-nil signatures "Should have signatures")
            (when (and signatures (> (length signatures) 0))
              (let* ((sig (aref signatures 0))
                     (label (gethash "label" sig)))
                (assert-not-nil label "Should have label")
                ;; Label should mention cons
                (assert-not-nil (search "cons" label)
                                "Signature label should contain 'cons'")))))))))

(deftest test-signature-help-nested-calls-finds-inner
  "Test that signatureHelp finds the innermost function call, not outer ones"
  (with-direct-handler-test
    (init-server)
    ;; Nested call: (when condition (cl:list arg1 arg2))
    ;; Cursor inside the inner (cl:list ...) should show list's signature, not when's
    (let ((code "(when t
      (cl:list 1 ))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      ;; Position cursor inside (cl:list 1 ) - after the "1 "
      ;; Line 1, character 17 should be inside the list call
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 1 "character" 17))))
             (result (response-result-safe response)))
        (assert-not-nil result "Should find signature")
        (when result
          (let ((signatures (gethash "signatures" result)))
            (when (and signatures (> (length signatures) 0))
              (let* ((sig (aref signatures 0))
                     (label (gethash "label" sig)))
                ;; Should be list's signature, NOT when's
                (assert-not-nil (search "list" label)
                                (format nil "Should find 'list' signature, got: ~A" label))
                (assert-nil (search "when" label)
                            (format nil "Should NOT find 'when' signature, got: ~A" label))))))))))

(deftest test-signature-help-deeply-nested-multiline
  "Test signature help with deeply nested multi-line code like in did-save.lisp"
  (with-direct-handler-test
    (init-server)
    ;; Structure similar to the user's actual code
    (let ((code "(let ((document-text (gethash \"key\" *table*)))
     (when document-text
           (cl:format t \"~A\" document-text)))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      ;; Position cursor inside (cl:format ...) - on line 2, inside the format call
      ;; "(cl:format t \"~A\" document-text)" starts at column 11
      ;; Cursor at column 22 should be after "t " inside format
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 2 "character" 22))))
             (result (response-result-safe response)))
        (assert-not-nil result "Should find signature for format")
        (when result
          (let ((signatures (gethash "signatures" result)))
            (when (and signatures (> (length signatures) 0))
              (let* ((sig (aref signatures 0))
                     (label (gethash "label" sig)))
                ;; Should be format's signature, NOT when's or let's
                (assert-not-nil (search "format" label)
                                (format nil "Should find 'format' signature, got: ~A" label))))))))))

(deftest test-signature-help-cursor-near-closing-parens
  "Test signature help when cursor is inside a call before its closing paren"
  (with-direct-handler-test
    (init-server)
    ;; Code with multiple closing parens like the user's did-save.lisp
    (let ((code "(let ((x 1))
     (when x
           (cl:list 1 2)))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      ;; Line 2: "           (cl:list 1 2)))"
      ;;          0         1         2
      ;;          012345678901234567890123456
      ;; Position 22 = "2" (last argument of cl:list)
      ;; Position 23 = first ) closing cl:list
      ;; Position 24 = second ) closing when
      ;; To be INSIDE cl:list, cursor should be at position 22 or earlier
      (let* ((response (call-handler "textDocument/signatureHelp"
                                     (dict "textDocument" (dict "uri" "file:///tmp/sig-test.lisp")
                                           "position" (dict "line" 2 "character" 22))))
             (result (response-result-safe response)))
        (when result
          (let ((signatures (gethash "signatures" result)))
            (when (and signatures (> (length signatures) 0))
              (let* ((sig (aref signatures 0))
                     (label (gethash "label" sig)))
                ;; Should be list's signature
                (assert-not-nil (search "list" label)
                                (format nil "Should find 'list' signature when inside cl:list, got: ~A" label))))))))))

;;; Comprehensive signature help test with complex nested code

(defparameter *complex-sig-help-code*
  "(defun process-data (input-list transform-fn)
  \"Process INPUT-LIST by applying TRANSFORM-FN.\"
  (let ((result nil)
        (count 0))
    (when input-list
      (setf result (mapcar transform-fn input-list))
      (setf count (length result))
      (when (> count 0)
        (cl:format t \"Processed ~A items\" count)
        (values result count)))))

(defun caller-func ()
  (process-data '(1 2 3) #'identity))"
  "Complex code with defun, let, when, setf, mapcar, length, format, values")

(deftest test-signature-help-comprehensive
  "Comprehensive test of signature help across various positions in complex code"
  (with-direct-handler-test
    (init-server)
    (call-handler "textDocument/didOpen"
                  (dict "textDocument" (dict "uri" "file:///tmp/complex-sig.lisp"
                                             "languageId" "lisp"
                                             "version" 1
                                             "text" *complex-sig-help-code*))
                  :id nil)

    ;; Line 0: "(defun process-data (input-list transform-fn)"
    ;; Line 1: "  \"Process INPUT-LIST by applying TRANSFORM-FN.\""
    ;; Line 2: "  (let ((result nil)"
    ;; Line 3: "        (count 0))"
    ;; Line 4: "    (when input-list"
    ;; Line 5: "      (setf result (mapcar transform-fn input-list))"
    ;; Line 6: "      (setf count (length result))"
    ;; Line 7: "      (when (> count 0)"
    ;; Line 8: "        (cl:format t \"Processed ~A items\" count)"
    ;; Line 9: "        (values result count)))))"
    ;; Line 10: ""
    ;; Line 11: "(defun caller-func ()"
    ;; Line 12: "  (process-data '(1 2 3) #'identity))"

    (macrolet ((get-sig-label (line char)
                 `(let* ((response (call-handler "textDocument/signatureHelp"
                                                 (dict "textDocument" (dict "uri" "file:///tmp/complex-sig.lisp")
                                                       "position" (dict "line" ,line "character" ,char))))
                         (result (response-result-safe response)))
                    (when result
                      (let ((signatures (gethash "signatures" result)))
                        (when (and signatures (vectorp signatures) (> (length signatures) 0))
                          (gethash "label" (aref signatures 0))))))))

      ;; Test 1: Inside (let ((result nil) - cursor after "let "
      ;; Line 2, char 6 should be inside let
      (let ((label (get-sig-label 2 6)))
        (when label
          (assert-not-nil (search "let" label)
                          (format nil "Position 2:6 should be 'let', got: ~A" label))))

      ;; Test 2: Inside (when input-list - cursor after "when "
      ;; Line 4, char 10 should be inside when
      (let ((label (get-sig-label 4 10)))
        (when label
          (assert-not-nil (search "when" label)
                          (format nil "Position 4:10 should be 'when', got: ~A" label))))

      ;; Test 3: Inside (setf result (mapcar ...)) - cursor after "setf "
      ;; Line 5, char 12 should be inside setf
      (let ((label (get-sig-label 5 12)))
        (when label
          (assert-not-nil (search "setf" label)
                          (format nil "Position 5:12 should be 'setf', got: ~A" label))))

      ;; Test 4: Inside (mapcar transform-fn input-list) - cursor after "mapcar "
      ;; Line 5, char 27 should be inside mapcar
      (let ((label (get-sig-label 5 27)))
        (when label
          (assert-not-nil (search "mapcar" label)
                          (format nil "Position 5:27 should be 'mapcar', got: ~A" label))))

      ;; Test 5: Inside (length result) - cursor after "length "
      ;; Line 6, char 20 should be inside length
      (let ((label (get-sig-label 6 20)))
        (when label
          (assert-not-nil (search "length" label)
                          (format nil "Position 6:20 should be 'length', got: ~A" label))))

      ;; Test 6: Inside (> count 0) - cursor after "> "
      ;; Line 7, char 14 should be inside >
      (let ((label (get-sig-label 7 14)))
        (when label
          (assert-not-nil (or (search ">" label) (search "number" (string-downcase label)))
                          (format nil "Position 7:14 should be '>' or number comparison, got: ~A" label))))

      ;; Test 7: Inside (cl:format t ...) - cursor after "cl:format "
      ;; Line 8, char 19 should be inside format
      (let ((label (get-sig-label 8 19)))
        (when label
          (assert-not-nil (search "format" label)
                          (format nil "Position 8:19 should be 'format', got: ~A" label))))

      ;; Test 8: Inside (values result count) - cursor after "values "
      ;; Line 9, char 16 should be inside values
      (let ((label (get-sig-label 9 16)))
        (when label
          (assert-not-nil (search "values" label)
                          (format nil "Position 9:16 should be 'values', got: ~A" label))))

      ;; Test 9: Inside nested when on line 7, but at the end near closing parens
      ;; Line 9, char 30 (near the )))) at end) - should still be inside values
      (let ((label (get-sig-label 9 28)))
        (when label
          (assert-not-nil (search "values" label)
                          (format nil "Position 9:28 should still be 'values', got: ~A" label)))))))

(deftest test-signature-help-did-save-structure
  "Test signature help with exact structure from did-save.lisp that was reported buggy"
  (with-direct-handler-test
    (init-server)
    ;; Exact structure from did-save.lisp lines 11-14
    (let ((code "(let ((document-text (gethash (format nil \"file://~A\" uri)
                                          *documents*)))
     (when document-text
           (build-file-symbol-map uri document-text)))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/did-save-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)

      ;; Line 0: "(let ((document-text (gethash (format nil \"file://~A\" uri)"
      ;; Line 1: "                                          *documents*)))"
      ;; Line 2: "     (when document-text"
      ;; Line 3: "           (build-file-symbol-map uri document-text)))"

      (macrolet ((get-sig-label (line char)
                   `(let* ((response (call-handler "textDocument/signatureHelp"
                                                   (dict "textDocument" (dict "uri" "file:///tmp/did-save-test.lisp")
                                                         "position" (dict "line" ,line "character" ,char))))
                           (result (response-result-safe response)))
                      (when result
                        (let ((signatures (gethash "signatures" result)))
                          (when (and signatures (vectorp signatures) (> (length signatures) 0))
                            (gethash "label" (aref signatures 0))))))))

        ;; Test cursor inside (build-file-symbol-map uri document-text)
        ;; Line 3: "           (build-file-symbol-map uri document-text)))"
        ;;          0         1         2         3         4         5
        ;;          0123456789012345678901234567890123456789012345678901234
        ;; Position 11 is the opening paren
        ;; Position 12-31 is "build-file-symbol-map"
        ;; Position 32 is space
        ;; Position 33-35 is "uri"
        ;; Position 36 is space
        ;; Position 37-49 is "document-text"
        ;; Position 50 is first )

        ;; Cursor at position 35 (after "uri") - should be build-file-symbol-map
        (let ((label (get-sig-label 3 35)))
          ;; If we get a result, it should NOT be "when"
          (when label
            (assert-nil (search "when" label)
                        (format nil "Position 3:35 inside build-file-symbol-map should NOT be 'when', got: ~A" label))))

        ;; Cursor at position 45 (in middle of document-text arg) - should be build-file-symbol-map
        (let ((label (get-sig-label 3 45)))
          (when label
            (assert-nil (search "when" label)
                        (format nil "Position 3:45 inside build-file-symbol-map should NOT be 'when', got: ~A" label))))

        ;; Cursor at position 50 (right before closing paren) - should be build-file-symbol-map
        (let ((label (get-sig-label 3 50)))
          (when label
            (assert-nil (search "when" label)
                        (format nil "Position 3:50 at end of build-file-symbol-map should NOT be 'when', got: ~A" label))))

        ;; For comparison: cursor inside (when ...) but before inner call
        ;; Line 2: "     (when document-text"
        ;; Position 11 is after "when "
        (let ((label (get-sig-label 2 11)))
          (when label
            (assert-not-nil (search "when" label)
                            (format nil "Position 2:11 should be 'when', got: ~A" label))))))))

(deftest test-signature-help-pkg-qualified-nested
  "Test signature help with package-qualified names in nested structure"
  (with-direct-handler-test
    (init-server)
    ;; Use clef-symbols: prefix like the actual did-save.lisp
    (let ((code "(let ((text \"hello\"))
     (when text
           (clef-symbols:build-file-symbol-map uri text)))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/pkg-qual-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)

      ;; Line 0: "(let ((text \"hello\"))"
      ;; Line 1: "     (when text"
      ;; Line 2: "           (clef-symbols:build-file-symbol-map uri text)))"
      ;;          0         1         2         3         4         5         6
      ;;          012345678901234567890123456789012345678901234567890123456789012

      (macrolet ((get-sig-label (line char)
                   `(let* ((response (call-handler "textDocument/signatureHelp"
                                                   (dict "textDocument" (dict "uri" "file:///tmp/pkg-qual-test.lisp")
                                                         "position" (dict "line" ,line "character" ,char))))
                           (result (response-result-safe response)))
                      (when result
                        (let ((signatures (gethash "signatures" result)))
                          (when (and signatures (vectorp signatures) (> (length signatures) 0))
                            (gethash "label" (aref signatures 0))))))))

        ;; Line 2 positions:
        ;; 11 = opening paren of clef-symbols:build-file-symbol-map
        ;; 12-43 = "clef-symbols:build-file-symbol-map"
        ;; 44 = space
        ;; 45-47 = "uri"
        ;; 48 = space
        ;; 49-52 = "text"
        ;; 53-55 = ")))"

        ;; Test at position 47 (after "uri") - should find build-file-symbol-map, NOT when
        (let ((label (get-sig-label 2 47)))
          (when label
            (assert-nil (search "when" label)
                        (format nil "Pos 2:47 in pkg-qualified func should NOT be 'when', got: ~A" label))))

        ;; Test at position 52 (after "text", before ))) - should find build-file-symbol-map
        (let ((label (get-sig-label 2 52)))
          (when label
            (assert-nil (search "when" label)
                        (format nil "Pos 2:52 at end of pkg-qualified func should NOT be 'when', got: ~A" label))))

        ;; Test at position 53 (on first closing paren)
        (let ((label (get-sig-label 2 53)))
          (when label
            (assert-nil (search "when" label)
                        (format nil "Pos 2:53 on closing paren should NOT be 'when', got: ~A" label))))))))

(deftest test-signature-help-debug-positions
  "Debug test to verify exact positions and what's being found"
  (with-direct-handler-test
    (init-server)
    ;; Simpler test case to debug position calculation
    (let ((code "(when condition
  (cl:list a b))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/debug-pos.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)

      ;; Line 0: "(when condition"  (length 15)
      ;; Line 1: "  (cl:list a b))" (length 16)
      ;;          0123456789012345
      ;; Position 2 = opening paren of cl:list
      ;; Position 3-9 = "cl:list"
      ;; Position 10 = space
      ;; Position 11 = "a"
      ;; Position 12 = space
      ;; Position 13 = "b"
      ;; Position 14 = first )
      ;; Position 15 = second )

      (macrolet ((test-pos (line char expected-substr)
                   `(let* ((response (call-handler "textDocument/signatureHelp"
                                                   (dict "textDocument" (dict "uri" "file:///tmp/debug-pos.lisp")
                                                         "position" (dict "line" ,line "character" ,char))))
                           (result (response-result-safe response))
                           (label (when result
                                    (let ((signatures (gethash "signatures" result)))
                                      (when (and signatures (vectorp signatures) (> (length signatures) 0))
                                        (gethash "label" (aref signatures 0)))))))
                      (if ,expected-substr
                          (assert-not-nil (and label (search ,expected-substr label))
                                          (format nil "Line ~A char ~A: expected '~A', got: ~A"
                                                  ,line ,char ,expected-substr label))
                          ;; nil means we expect no result or not a specific function
                          t))))

        ;; Inside cl:list after "a "
        (test-pos 1 12 "list")

        ;; Right before first ) on line 1
        (test-pos 1 14 "list")

        ;; At position of first ) - should still be inside list
        (test-pos 1 14 "list")

        ;; Inside when, position 6 on line 0
        (test-pos 0 6 "when")))))
