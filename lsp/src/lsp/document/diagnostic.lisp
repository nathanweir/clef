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

;;; ---------------------------------------------------------------------------
;;; Syntax errors come from tree-sitter, not from SBCL.
;;;
;;; Deliberate: reader errors carry no usable position. A truncated form signals
;;; END-OF-FILE with a NIL file-position, so there is nothing to report against.
;;; Tree-sitter, being error-tolerant, locates them precisely. See
;;; docs/surveys/w0-conditions.md.
;;; ---------------------------------------------------------------------------

(defun get-syntax-errors (input-text)
       "Parse Lisp source code and emit a Diagnostic for each syntax error."
       (let* ((tree (clef-parser/parser:parse-string input-text))
              (errors (collect-error-nodes tree))
              (diagnostics '()))
             (dolist (err errors)
                     (destructuring-bind ((start-col start-line) (end-col end-line)) (cdr err)
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
             (return-from handle-text-document-diagnostic
                          (dict "kind" "full" "items" #())))

       (let* ((document-uri (href (clef-jsonrpc/types:request-params message)
                                  "text-document"
                                  "uri"))
              (document-text (gethash document-uri ctx:documents))
              (syntax-errors (get-syntax-errors document-text))
              (compile-errors (collect-compile-diagnostics document-text))
              (items (append syntax-errors compile-errors)))
             (dict "kind" "full"
                   "items" (if items items #()))))

;;; ---------------------------------------------------------------------------
;;; Semantic diagnostics, via clef-conditions.
;;;
;;; This used to regex-scrape SBCL's printed English to recover the offending
;;; symbol, then search the raw source text for every substring match. That was
;;; wrong in three ways: "FOO" matched inside "FOO-BAR", occurrences inside
;;; strings and comments were flagged, and the search covered the whole file even
;;; though the condition belongs to one top-level form.
;;;
;;; Now: clef-conditions:extract gives the symbol as data and the enclosing
;;; top-level form as an index, and we find the symbol's actual token nodes
;;; within that form's subtree.
;;;
;;; Reporting EVERY occurrence is intentional and tested -- a symbol that is
;;; undefined is undefined at each of its uses.
;;; ---------------------------------------------------------------------------

(defun node-kind (node)
       "Tree-sitter node kinds arrive either bare (:LIST-LIT) or paired with a
field name ((:VALUE :SYM-LIT)). Normalise to the kind."
       (let ((k (first node)))
            (if (consp k) (second k) k)))

(defun toplevel-forms (tree)
       "Top-level forms in TREE, excluding comments.

Comments are children of :SOURCE but are not forms, and SBCL's source path
indexes forms as read -- so counting comments would misalign every index."
       (remove-if (lambda (n) (eq (node-kind n) :comment))
                  (ts:node-children tree)))

(defun toplevel-index (source-path)
       "SBCL's ORIGINAL-SOURCE-PATH is innermost-first, so its LAST element is the
index of the top-level form. The earlier elements walk down into the form; we
deliberately do not follow them -- see the note in
docs/surveys/w0-conditions.md."
       (when (and source-path (listp source-path))
             (let ((last (car (last source-path))))
                  (when (integerp last) last))))

(defun symbol-nodes-in (node symbol-name source)
       "Every :SYM-LIT node under NODE whose text names SYMBOL-NAME.

Matching whole tokens rather than substrings, which is what makes this immune to
the FOO/FOO-BAR confusion, and it never looks inside strings or comments because
those are different node kinds."
       (let ((found '())
             (target (string-upcase symbol-name)))
            (labels ((walk (n)
                           (when n
                                 (when (eq (node-kind n) :sym-lit)
                                       (let ((text (ignore-errors
                                                    (clef-parser/parser:node-text n source))))
                                            (when (and text
                                                       (string= (string-upcase
                                                                 (strip-package-prefix text))
                                                                target))
                                                  (push n found))))
                                 (dolist (child (ts:node-children n))
                                         (walk child)))))
                    (walk node))
            (nreverse found)))

(defun strip-package-prefix (text)
       "FOO:BAR and FOO::BAR both name BAR."
       (let ((colon (position #\: text :from-end t)))
            (if colon (subseq text (1+ colon)) text)))

(defun severity-for (extracted)
       (ecase (clef-conditions:diagnostic-severity extracted)
              (:error +diagnostic-severity-error+)
              ;; Style warnings are warnings to an editor. Unused variables are
              ;; the common case and a test pins them at severity 2.
              ((:warning :style-warning) +diagnostic-severity-warning+)
              (:note +diagnostic-severity-information+)))

(defun node-to-range (node)
       (make-range (clef-parser/parser:node-start-point-row node)
                   (clef-parser/parser:node-start-point-column node)
                   (clef-parser/parser:node-end-point-row node)
                   (clef-parser/parser:node-end-point-column node)))

(defun diagnostics-for (extracted tree source)
       "Turn one extracted diagnostic into LSP diagnostics -- one per occurrence."
       (let* ((severity (severity-for extracted))
              (message (clef-conditions:diagnostic-message extracted))
              (sym (clef-conditions:diagnostic-symbol extracted))
              (index (toplevel-index (clef-conditions:diagnostic-source-path extracted)))
              (forms (toplevel-forms tree))
              (form (when (and index (< index (length forms))) (nth index forms))))
             (cond
               ;; Best case: we know the form and the symbol. Report each use.
               ((and form sym)
                (let ((nodes (symbol-nodes-in form (symbol-name sym) source)))
                     (if nodes
                         (mapcar (lambda (n)
                                         (dict "range" (node-to-range n)
                                               "severity" severity
                                               "message" message))
                                 nodes)
                         ;; The symbol is not spelled in this form -- a macro
                         ;; expansion, most likely. Fall back to the form.
                         (list (dict "range" (node-to-range form)
                                     "severity" severity
                                     "message" message)))))
               ;; We know the form but not which symbol. The form is honest.
               (form
                 (list (dict "range" (node-to-range form)
                             "severity" severity
                             "message" message)))
               ;; No location at all: runtime conditions, reader errors.
               (t
                 (list (dict "range" (make-range 0 0 0 0)
                             "severity" severity
                             "message" message))))))

(defun reportable-condition-p (c)
       "Is C something the editor should hear about?

Note SB-C:COMPILER-ERROR explicitly. It is what a read error arrives as -- an
unknown package prefix, say -- and it is *not* a subtype of ERROR, so filtering
on (or warning error) drops it and the file reports nothing at all."
       (typep c '(or warning error sb-c:compiler-error)))

(defun filter-condition (condition)
       "Filter out specific diagnostics conditions. Essentialy just a hardcoded whitelist. Returns true if the message should be filtered"
       ;; Filter out warnings like 'redefining * in DEFMACRO'
       (let ((cond-text (princ-to-string condition)))
            (and (search "redefining" cond-text)
                 (search "DEFMACRO" cond-text))))

(defun collect-compile-diagnostics (source-string)
       "Compile SOURCE-STRING and report what the compiler complains about."
       (let* ((tree (clef-parser/parser:parse-string source-string))
              (source-package (or (clef-parser/utils:find-package-declaration tree source-string)
                                  *package*))
              (*package* source-package)
              (output (make-string-output-stream))
              (extracted '()))
             ;; HANDLER-CASE outside, HANDLER-BIND inside, deliberately.
             ;;
             ;; A read error aborts the whole compilation unit -- a bad package
             ;; prefix, say -- and SBCL signals it as a fatal COMPILER-ERROR.
             ;; Letting that propagate would discard every diagnostic already
             ;; collected, so the file with the mistake in it reports nothing at
             ;; all. Nesting this way means the collecting handler still runs
             ;; first (it is the more recently established), and only then does
             ;; the outer form swallow the error and let us return what we have.
             (handler-case
               (unwind-protect
                 (handler-bind
                   ((condition
                      (lambda (c)
                              ;; Extraction must happen inside the handler: the
                              ;; compiler's error context is dynamic state and is
                              ;; gone the moment this returns.
                              (when (and (reportable-condition-p c)
                                         (not (filter-condition c)))
                                    (push (clef-conditions:extract c) extracted)))))
                   (uiop:call-with-temporary-file
                     (lambda (stream temp-path)
                             (write-string source-string stream)
                             (force-output stream)
                             (close stream)
                             (let ((*standard-output* output)
                                   (*error-output* output)
                                   (*trace-output* output))
                                  (let ((fasl-path (compile-file temp-path :verbose nil :print nil)))
                                       (when (and fasl-path (probe-file fasl-path))
                                             (delete-file fasl-path)))))
                     :want-stream-p t
                     :want-pathname-p t
                     :type "lisp"
                     :keep nil))
                 (slog :debug "compile-file output: ~%~A" (get-output-stream-string output)))
               (error (c)
                      (slog :debug "compilation aborted: ~A" c)))

             (loop for e in (nreverse extracted)
                   append (diagnostics-for e tree source-string))))
