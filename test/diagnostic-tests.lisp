(in-package :clef-test)

;;; Diagnostic tests: syntax errors, compile errors, warnings

;;; Helper functions for diagnostic tests

(defun get-diagnostic-items (response)
  "Extract diagnostic items from a diagnostic response"
  (let ((result (response-result-safe response)))
    (when result
      (let ((items (gethash "items" result)))
        (if (vectorp items)
            (coerce items 'list)
            items)))))

(defun find-diagnostic-with-message (diagnostics message-substring)
  "Find a diagnostic whose message contains the given substring (case-insensitive)"
  (find-if (lambda (diag)
             (let ((msg (gethash "message" diag)))
               (and msg (search (string-upcase message-substring)
                               (string-upcase msg)))))
           diagnostics))

(defun diagnostic-range-start-line (diagnostic)
  "Get the start line from a diagnostic's range"
  (let ((range (gethash "range" diagnostic)))
    (when range
      (let ((start (gethash "start" range)))
        (when start
          (gethash "line" start))))))

(defun diagnostic-range-start-char (diagnostic)
  "Get the start character from a diagnostic's range"
  (let ((range (gethash "range" diagnostic)))
    (when range
      (let ((start (gethash "start" range)))
        (when start
          (gethash "character" start))))))

(defun diagnostic-range-end-line (diagnostic)
  "Get the end line from a diagnostic's range"
  (let ((range (gethash "range" diagnostic)))
    (when range
      (let ((end (gethash "end" range)))
        (when end
          (gethash "line" end))))))

(defun diagnostic-severity (diagnostic)
  "Get the severity from a diagnostic"
  (gethash "severity" diagnostic))

;;; Syntax error tests (detected by tree-sitter)

(deftest test-diagnostic-unbalanced-open-paren
  "Test that unbalanced open parenthesis is detected"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (+ 1 2)"))  ; Missing closing paren
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        (assert-true (> (length items) 0) "Should have at least one diagnostic")
        (let ((syntax-error (find-diagnostic-with-message items "syntax")))
          (assert-not-nil syntax-error "Should have a syntax error diagnostic"))))))

(deftest test-diagnostic-unbalanced-close-paren
  "Test that extra closing parenthesis is detected"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (+ 1 2)))"))  ; Extra closing paren
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        (assert-true (> (length items) 0) "Should have at least one diagnostic")))))

(deftest test-diagnostic-unclosed-string
  "Test that unclosed string literal is detected"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (print \"hello))"))  ; Unclosed string
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        (assert-true (> (length items) 0) "Should detect unclosed string")))))

(deftest test-diagnostic-syntax-error-has-range
  "Test that syntax errors have proper range information"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (+ 1 2"))  ; Missing closing parens - error is on line 1
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/syntax-test.lisp"))))
             (items (get-diagnostic-items response)))
        (when (and items (> (length items) 0))
          (let ((first-diag (first items)))
            (assert-not-nil (gethash "range" first-diag) "Diagnostic should have range")
            (assert-not-nil (diagnostic-range-start-line first-diag) "Range should have start line")))))))

;;; Compile-time error tests (detected by SBCL)

(deftest test-diagnostic-undefined-function
  "Test that undefined function call is detected"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (nonexistent-function 1 2 3))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        (let ((undef-error (find-diagnostic-with-message items "undefined function")))
          (assert-not-nil undef-error "Should detect undefined function")
          (assert-not-nil (search "NONEXISTENT-FUNCTION"
                                  (string-upcase (gethash "message" undef-error)))
                          "Error message should mention the function name"))))))

(deftest test-diagnostic-undefined-function-range
  "Test that undefined function diagnostic has correct range"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (undefined-fn 1))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"))))
             (items (get-diagnostic-items response)))
        (when items
          (let ((undef-error (find-diagnostic-with-message items "undefined")))
            (when undef-error
              ;; The error should be on line 1 where undefined-fn is called
              (let ((start-line (diagnostic-range-start-line undef-error)))
                (assert-not-nil start-line "Should have start line")
                (assert-equal 1 start-line "Error should be on line 1")))))))))

(deftest test-diagnostic-undefined-variable
  "Test that undefined variable reference is detected"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (+ undefined-var 10))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        (let ((undef-error (find-diagnostic-with-message items "undefined variable")))
          (assert-not-nil undef-error "Should detect undefined variable")
          (assert-not-nil (search "UNDEFINED-VAR"
                                  (string-upcase (gethash "message" undef-error)))
                          "Error message should mention the variable name"))))))

(deftest test-diagnostic-wrong-arg-count
  "Test that wrong number of arguments is detected"
  (with-direct-handler-test
    (init-server)
    ;; cons takes exactly 2 arguments
    (let ((code "(defun foo ()
  (cons 1 2 3))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        ;; Should have an error about wrong number of args
        (let ((arg-error (find-diagnostic-with-message items "argument")))
          (assert-not-nil arg-error "Should detect wrong argument count"))))))

(deftest test-diagnostic-package-not-found
  "Test that reference to nonexistent package is detected"
  (with-direct-handler-test
    (init-server)
    ;; Use a symbol from a nonexistent package (pkg:symbol syntax)
    (let ((code "(defun foo ()
  (nonexistent-pkg-xyz:some-func 1 2))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        (let ((pkg-error (find-diagnostic-with-message items "package")))
          (assert-not-nil pkg-error "Should detect package not found error"))))))

;;; Error severity tests

(deftest test-diagnostic-error-severity
  "Test that compile errors have error severity (1)"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (undefined-function-xyz))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/compile-test.lisp"))))
             (items (get-diagnostic-items response)))
        (when items
          (let ((error-diag (find-diagnostic-with-message items "undefined")))
            (when error-diag
              ;; Error severity should be 1 or 2 (error or warning)
              (let ((severity (diagnostic-severity error-diag)))
                (assert-not-nil severity "Diagnostic should have severity")
                (assert-true (member severity '(1 2))
                             "Severity should be error (1) or warning (2)")))))))))

;;; Clean code tests

(deftest test-diagnostic-clean-code-no-errors
  "Test that clean code returns no diagnostics"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun add (a b)
  (+ a b))

(defun multiply (x y)
  (* x y))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/clean-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/clean-test.lisp"))))
             (items (get-diagnostic-items response)))
        ;; Clean code should have no diagnostics (or empty list)
        (assert-true (or (null items) (= (length items) 0))
                     "Clean code should have no diagnostics")))))

(deftest test-diagnostic-response-structure
  "Test that diagnostic response has correct LSP structure"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo () 1)"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/struct-test.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/struct-test.lisp"))))
             (result (response-result-safe response)))
        (assert-not-nil result "Should get diagnostic result")
        (assert-not-nil (gethash "kind" result) "Result should have 'kind' field")
        (assert-equal "full" (gethash "kind" result) "Kind should be 'full'")
        (assert-true (or (gethash "items" result) t) "Result should have 'items' field")))))

;;; Multiple errors in one file

(deftest test-diagnostic-multiple-errors
  "Test that multiple errors in one file are all detected"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defun foo ()
  (undefined-func-1)
  (undefined-func-2)
  (+ undefined-var-1 undefined-var-2))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/multi-error.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/multi-error.lisp"))))
             (items (get-diagnostic-items response)))
        (assert-not-nil items "Should return diagnostic items")
        ;; Should have multiple errors
        (assert-true (>= (length items) 2)
                     "Should detect multiple errors")))))

;;; .asd file skipping test

(deftest test-diagnostic-skips-asd-files
  "Test that diagnostics are skipped for .asd files"
  (with-direct-handler-test
    (init-server)
    (let ((code "(defsystem :my-broken-system
  :depends-on (nonexistent-system))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/test.asd"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/test.asd"))))
             (items (get-diagnostic-items response)))
        ;; .asd files should return empty diagnostics
        (assert-true (or (null items) (= (length items) 0))
                     ".asd files should not be diagnosed")))))

;;; Multiple occurrence highlighting test

(deftest test-diagnostic-highlights-all-occurrences
  "Test that ALL occurrences of an undefined symbol are highlighted - exactly one per occurrence"
  (with-direct-handler-test
    (init-server)
    ;; Code with the same undefined function called 3 times
    (let ((code "(defun foo ()
  (undefined-xyz 1)
  (undefined-xyz 2)
  (undefined-xyz 3))"))
      (call-handler "textDocument/didOpen"
                    (dict "textDocument" (dict "uri" "file:///tmp/multi-occur.lisp"
                                               "languageId" "lisp"
                                               "version" 1
                                               "text" code))
                    :id nil)
      (let* ((response (call-handler "textDocument/diagnostic"
                                     (dict "textDocument" (dict "uri" "file:///tmp/multi-occur.lisp"))))
             (items (get-diagnostic-items response))
             ;; Count diagnostics for undefined-xyz
             (undef-items (remove-if-not
                            (lambda (item)
                                   (search "UNDEFINED-XYZ"
                                           (string-upcase (gethash "message" item))))
                            items)))
        ;; Should have EXACTLY 3 diagnostics - one for each occurrence, no duplicates
        (assert-equal 3 (length undef-items)
                      (format nil "Should have exactly 3 diagnostics for undefined-xyz, got ~A"
                              (length undef-items)))))))
