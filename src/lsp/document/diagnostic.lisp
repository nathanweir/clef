(in-package :clef-lsp/document)

;; TODO: I was originally putting all of these types into :clef-lsp/types, but I dislike that now
;; that I've (for now) given up on fully replicating the LSP spec types in CLOS. Consider where this
;; should live long-term.

(defconstant +diagnostic-severity-error+ 1 "Reports an error.")
(defconstant +diagnostic-severity-warning+ 2 "Reports a warning.")
(defconstant +diagnostic-severity-information+ 3 "Reports an information.")
(defconstant +diagnostic-severity-hint+ 4 "Reports a hint.")

;; Utils; need to be place elsewhere and long-term turned into classes/structs/Coalton types (something)

(defun make-position (line char)
       (dict "line" line "character" char))

(defun make-range (start-line start-char end-line end-char)
       (dict "start" (make-position start-line start-char)
             "end" (make-position end-line end-char)))

(defun get-syntax-errors (input-text)
       "Parse Lisp source code and emit a Diagnostic for each syntax error."
       (let* ((tree (clef-parser/parser:parse-string input-text))
              (errors (collect-error-nodes tree))
              (diagnostics '()))
             (dolist (err errors)
                     (slog :debug "Err is ~A" err)
                     (destructuring-bind ((start-col start-line) (end-col end-line)) (cdr err)
                                         (slog :debug "[textDocument/diagnostic] Found syntax error from ~A:~A to ~A:~A" start-line start-col end-line end-col)
                                         (push (dict "range" (make-range start-col start-line end-line end-col)
                                                     "severity" +diagnostic-severity-error+
                                                     "message" "Syntax error")
                                               diagnostics)))
             diagnostics))

(defun collect-error-nodes (node)
       "Return a list of (TYPE RANGE) for all error nodes in the tree."
       (let ((results '()))
            (labels ((walk (n)
                           (let ((type (ts:node-type n)))
                                (when (or (eql type :error) (eql type :missing))
                                      (push (cons type (ts:node-range n)) results))
                                (dolist (child (ts:node-children n))
                                        (walk child)))))
                    (walk node))
            results))

(defun handle-text-document-diagnostic (message)
       ;; Ignore computing diag on .asd files
       (when (serapeum:string-suffix-p ".asd"
                                       (href (clef-jsonrpc/types:request-params message)
                                             "text-document"
                                             "uri"))
             (slog :debug "[textDocument/diagnostic] Skipping diagnostics for .asd file")
             (return-from handle-text-document-diagnostic
                          (dict "kind" "full" "items" #())))

       (let* ((document-uri (href (clef-jsonrpc/types:request-params message)
                                  "text-document"
                                  "uri"))
              (document-text (gethash document-uri clef-lsp/server:*documents*))
              (syntax-errors (get-syntax-errors document-text))
              (compile-errors (debounced-sb-collect-diagnostics document-text))
              (items (append syntax-errors compile-errors)))
             (slog :debug "[textDocument/diagnostic] Reporting ~A diagnostics for ~A" (length items) document-uri)
             (slog :debug "[textDocument/diagnostic] Items: ~A" items)
             (dict "kind" "full"
                   "items" (if items items #()))))

;; The below is extremely sloppy and dubious LLM-driven code for compiling documents and extracting their errors.
;; Could use a full rewrite
(defun debounced-sb-collect-diagnostics (input-text)
       "Uses SBCL to get diagnostics for a given file. TODO: Debounce this to 500ms"
       (let* ((tree (clef-parser/parser:parse-string input-text))
              (symbol-map (build-symbol-index tree input-text)))
             ;; (maphash (lambda (k v)
             ;;              (format t "symbol-map: ~A => ~A~%" k v))
             ;;          symbol-map)

             ;; print all values of the symbol-map hash table
             ;; (slog :debug "[textDocument/diagnostic] Built symbol map with ~A entries" (clef-util:shallow-hash-vals symbol-map))
             (compile-and-collect-diagnostics symbol-map input-text tree)))

(defun compile-and-collect-diagnostics (symbol-map source-string tree)
       (let* ((source-package (or (clef-parser/utils:find-package-declaration tree source-string)
                                  *package*))
              (*package* source-package)
              (diagnostics '()))
             (block compile-block
                    (handler-bind
                      ((warning
                         (lambda (c)
                                 (when (not (filter-condition c))
                                       (push (make-diagnostic-from-condition c +diagnostic-severity-warning+ symbol-map source-string)
                                             diagnostics))
                                 (muffle-warning c)))
                       ;; Catch ALL conditions (including the compiler errors)
                       (condition
                         (lambda (c)
                                 ;; (format t "Intercepted condition: ~A (~A)~%" c (type-of c))
                                 ;; Record the diagnostic
                                 (when (not (filter-condition c))
                                       (push (make-diagnostic-from-condition c +diagnostic-severity-error+ symbol-map source-string)
                                             diagnostics))
                                 ;; Swallow the error by returning from the block
                                 (return-from compile-block nil))))
                      (uiop:call-with-temporary-file
                        (lambda (stream temp-path)
                                (write-string source-string stream)
                                (force-output stream)
                                (close stream)

                                (let ((fasl-path (compile-file temp-path :verbose nil :print nil)))
                                     (when (and fasl-path (probe-file fasl-path))
                                           (delete-file fasl-path))))
                        :want-stream-p t
                        :want-pathname-p t
                        :type "lisp"
                        :keep nil)))
             diagnostics))

(defun extract-range (node)
       "Extract LSP range from a cl-tree-sitter node."
       (when node
             (make-range (clef-parser/parser:node-start-point-row node)
                         (clef-parser/parser:node-start-point-column node)
                         (clef-parser/parser:node-end-point-row node)
                         (clef-parser/parser:node-end-point-column node))))


(defun make-diagnostic-from-condition (condition severity symbol-map source-string)
       "Create one or more diagnostics from a condition with proper range extraction."
       (let* ((symbol-name (extract-symbol-from-condition condition))
              (node (when symbol-name
                          (find-node-for-symbol symbol-name symbol-map)))
              (range (if node
                         (extract-range node)
                         (find-symbol-in-source source-string symbol-name))))

             ;; (format t "symbol-name is ~A~%" symbol-name)
             ;; (format t "Node is ~A~%" node)
             ;; (format t "Range is ~A~%" range)
             ;; (range message filename severity)
             ;; TODO: Extract to a "make-diagnostic"
             (dict "range" (or range
                               ;; Ultimate fallback
                               (make-range 0 0 0 0))
                   "severity" severity
                   "message" (princ-to-string condition))))

(defun filter-condition (condition)
       "Filter out specific diagnostics conditions. Essentialy just a hardcoded whitelist. Returns true if the message should be filtered"
       ;; Filter out warnings like 'redefining * in DEFMACRO'
       (let ((cond-text (princ-to-string condition)))
            (and (search "redefining" cond-text)
                 (search "DEFMACRO" cond-text))))

;; TODO: This is highly suspect, I don't think it works well. Revisit
;; Wait, am I just overwriting the entry with the most recent ocurrence of that symbol?
(defun build-symbol-index (tree source)
       "Build a hash table mapping symbol names to their tree-sitter nodes."
       (let ((symbol-map (make-hash-table :test 'equal)))
            (labels ((visit-node (node)
                                 (when node
                                       (let ((kind (first node)))
                                            (when (listp kind)
                                                  (when (and (= (list-length kind) 2)
                                                             (eq (second kind) :SYM-LIT))
                                                        (let ((text (and node (clef-parser/parser:node-text node source))))
                                                             (when (and text (not (gethash text symbol-map)))
                                                                   ;; Capitalize as it will be caps in the compile error
                                                                   (setf (gethash (string-upcase text) symbol-map) node)))))
                                            (dolist (child (ts:node-children node))
                                                    (when child (visit-node child)))))))
                    (when tree (visit-node tree))
                    symbol-map)))

(defun extract-symbol-from-condition (condition)
       "Extract the problematic symbol name from SBCL condition."
       ;; TODO: Many of these errors do contain line/char pos info of the form:
       ;; Line: 60, Column: 78, File-Position: 4046 (etc)
       ;; We should use that info instead of doing a symbol lookup after this
       (let ((message (princ-to-string condition)))
            ;; (format t "Message from condition is: ~A~%" message)
            (cond
              ;; Undefined function: FOO
              ((search "undefined function" message)
               (let* ((start (search ":" message))
                      (symbol-str (when start
                                        (string-trim '(#\Space #\Newline #\Tab)
                                                     (subseq message (1+ start))))))
                     (when symbol-str
                           ;; Handle package-qualified names
                           (let ((colon-pos (position #\: symbol-str :from-end t)))
                                (if colon-pos
                                    (subseq symbol-str (1+ colon-pos))
                                    symbol-str)))))
              ;; Undefined variable: BAR
              ((search "undefined variable" message)
               (let* ((start (search ":" message))
                      (symbol-str (when start
                                        (string-trim '(#\Space #\Newline #\Tab)
                                                     (subseq message (1+ start))))))
                     symbol-str))
              ;; Wrong number of params; "The function <symbol> is called with"
              ;; TODO: Can this cond be made wihtout repeating the func?
              ((cl-ppcre:scan-to-strings "The function ([\\w\\-]+) is called with" message)
               (let* ((matches (cl-ppcre:register-groups-bind (func-name)
                                                              ("The function ([\\w\\-]+) is called with" message)
                                                              func-name)))
                     matches))
              ;; Package <x> does not exist
              ((cl-ppcre:scan-to-strings "Package ([\\w\\-]+) does not exist" message)
               (let* ((matches (cl-ppcre:register-groups-bind (pkg-name)
                                                              ("Package ([\\w\\-]+) does not exist" message)
                                                              pkg-name)))
                     matches))
              ;; Symbol "SOURCES-BY-NAME" not found in the SB-INTROSPECT package
              ((cl-ppcre:scan-to-strings "Symbol \"([\\w\\-]+)\" not found in the [\\w\\-]+ package" message)
               (let* ((matches (cl-ppcre:register-groups-bind (sym-name)
                                                              ("Symbol \"([\\w\\-]+)\" not found in the [\\w\\-]+ package" message)
                                                              sym-name)))
                     matches))
              ;; Type error patterns
              ((search "type-error" (string-downcase message))
               ;; Try to extract symbol from type error message
               ;; This is trickier and may need custom parsing
               nil)
              (t nil))))

(defun find-node-for-symbol (symbol-name symbol-map)
       "Find the tree-sitter node for a given symbol name."
       (gethash (string-upcase symbol-name) symbol-map))



(defun find-symbol-in-source (source-string symbol-name)
       "Fallback: search for symbol in source text and return approximate range."
       (when symbol-name
             (let ((pos (search (string-upcase symbol-name)
                                (string-upcase source-string))))
                  (when pos
                        (multiple-value-bind (line column)
                                             (position-to-line-column source-string pos)
                                             (make-range line column line (+ column (length symbol-name))))))))

(defun position-to-line-column (string pos)
       "Convert character position to line/column."
       (let ((line 0)
             (column 0))
            (dotimes (i pos)
                     (if (char= (char string i) #\Newline)
                         (progn (incf line) (setf column 0))
                         (incf column)))
            (values line column)))
