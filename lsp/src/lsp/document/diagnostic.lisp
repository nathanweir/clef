(in-package :clef-lsp/document)

;; TODO: I was originally putting all of these types into :clef-lsp/types, but I dislike that now
;; that I've (for now) given up on fully replicating the LSP spec types in CLOS. Consider where this
;; should live long-term.

(defconstant +diagnostic-severity-error+ 1 "Reports an error.")
(defconstant +diagnostic-severity-warning+ 2 "Reports a warning.")
(defconstant +diagnostic-severity-information+ 3 "Reports an information.")
(defconstant +diagnostic-severity-hint+ 4 "Reports a hint.")

;; MAKE-POSITION, MAKE-RANGE and NODE-TO-RANGE used to be defined here. They now
;; live in :clef-lsp/types/basic and are imported -- see the note there.

;;; ---------------------------------------------------------------------------
;;; Syntax errors come from tree-sitter, not from SBCL.
;;;
;;; The reason is NOT that SBCL cannot locate them. An earlier version of this
;;; comment said reader errors carry no usable position; that was wrong, and
;;; clef-conditions now extracts one -- see
;;; docs/experiments/conditions/03-reader-error-api.lisp.
;;;
;;; The reason is that the reader stops at the first error and tree-sitter does
;;; not. One unbalanced paren ends SBCL's view of the file; tree-sitter, being
;;; error-tolerant, reports that error AND everything after it. For a buffer
;;; being typed into, that difference is the whole game.
;;;
;;; SBCL's reader errors still reach the editor through the compile path below,
;;; where they now arrive with a location and a message free of SBCL's
;;; "Stream: #<FORM-TRACKING-STREAM ...>" trailer.
;;; ---------------------------------------------------------------------------

(defun get-syntax-errors (input-text)
       "Parse Lisp source code and emit a Diagnostic for each syntax error."
       (let ((tree (clef-parser/parser:parse-string input-text))
             (diagnostics '()))
            (dolist (node (collect-error-nodes tree))
                    (push (dict "range" (node-to-range node)
                                "severity" +diagnostic-severity-error+
                                "message" "Syntax error")
                          diagnostics))
            diagnostics))

(defun collect-error-nodes (node)
       "Every :ERROR and :MISSING node in the tree.

Returns the nodes themselves, not their ranges. Ranges are built by
NODE-TO-RANGE at the point of use: cl-tree-sitter's raw ranges are
column-first, and destructuring them here is what used to put the column in
the range's \"line\" and the line in its \"character\"."
       (let ((results '()))
            (labels ((walk (n)
                           (let ((type (ts:node-type n)))
                                (when (or (eql type :error) (eql type :missing))
                                      (push n results))
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
index of the top-level form."
       (when (and source-path (listp source-path))
             (let ((last (car (last source-path))))
                  (when (integerp last) last))))

;;; ---------------------------------------------------------------------------
;;; Walking ORIGINAL-SOURCE-PATH into the tree
;;;
;;; The path's earlier elements walk down into the form, and following them is
;;; what gets the exact subexpression rather than the whole definition. Measured
;;; in docs/experiments/conditions/04-source-path-shape.lisp:
;;;
;;;   - It is innermost-first. (3 3 5) means top-level form 5, its element 3,
;;;     then that element's element 3.
;;;   - Indices are positional within the enclosing list, counting the operator
;;;     as element 0 -- exactly how NTH would index the form as read.
;;;   - Macroexpansion does not inject phantom entries. The path for an error
;;;     inside a macroexpansion stops at the macro *call site* in the original
;;;     source, so every index it contains addresses real source text.
;;;
;;; The catch is that this grammar does not parse every form as a flat list, so
;;; the Nth child is not always the Nth element. See FORM-ELEMENTS.
;;; ---------------------------------------------------------------------------

(defparameter +opaque-node-kinds+
  '(:loop-macro :quoting-lit :syn-quoting-lit :unquoting-lit :unquote-splicing-lit
    :var-quoting-lit :meta-lit :old-meta-lit :dis-expr :read-cond-lit
    :splicing-read-cond-lit :include-reader-macro)
  "Nodes whose children do not correspond 1:1 to the elements SBCL read.

A quote is the clearest case: 'X is one grammar node with one child, but reads as
(QUOTE X), two elements. LOOP has a whole clause grammar of its own where the
read form is a flat list of symbols. Reader conditionals may read as nothing at
all. Indexing into any of these would silently point at the wrong thing, so the
walk stops and the caller falls back.")

(defun form-elements (node)
       "The positional elements of the form at NODE, in the order SBCL indexes
them, or NIL when this node has no 1:1 correspondence to a read form.

Two grammar shapes need undoing. A top-level (defun ...) parses as a :LIST-LIT
wrapping a single :DEFUN that spans the same text, so the wrapper is transparent.
And :DEFUN bundles the keyword, name and lambda list into a :DEFUN-HEADER child,
where SBCL counts them as plain elements 0, 1 and 2 -- so the header is flattened
back out. The same shape covers DEFMACRO, DEFMETHOD and LAMBDA; for LAMBDA the
name is simply absent from the header, which keeps the indices aligned by itself."
       (let ((kind (node-kind node)))
            (cond
              ((member kind +opaque-node-kinds+) nil)
              ((eq kind :defun)
               (loop for child in (ts:node-children node)
                     unless (eq (node-kind child) :comment)
                       append (if (eq (node-kind child) :defun-header)
                                  (remove :comment (ts:node-children child)
                                          :key #'node-kind)
                                  (list child))))
              ((member kind '(:list-lit :vec-lit))
               (let ((children (remove :comment (ts:node-children node)
                                       :key #'node-kind)))
                    (if (and (= 1 (length children))
                             (member (node-kind (first children))
                                     '(:defun :loop-macro)))
                        (form-elements (first children))
                        children)))
              ;; Atoms, and anything the grammar shapes in a way we have not
              ;; verified. Better to fall back than to index into a guess.
              (t nil))))

(defun resolve-source-path (tree source-path)
       "The tree-sitter node for the exact subform SOURCE-PATH names, or NIL.

NIL is a normal outcome, not a failure: it means the path led somewhere this
grammar shapes differently, and the caller should fall back to the whole form."
       (let ((path (reverse source-path)))
            (when (and path (integerp (first path)))
                  (let ((forms (toplevel-forms tree)))
                       (when (< (first path) (length forms))
                             (let ((node (nth (first path) forms)))
                                  (dolist (index (rest path) node)
                                          (let ((elements (when (integerp index)
                                                                (form-elements node))))
                                               (unless (and elements
                                                            (< index (length elements)))
                                                       (return nil))
                                               (setf node (nth index elements))))))))))

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

(defparameter +form-scoped-kinds+
  '(:undefined :undefined-function :undefined-variable :undefined-type)
  "Kinds SBCL signals once per top-level form rather than once per occurrence.

Measured in docs/experiments/conditions/07-undefined-grouping.lisp. Three calls
to one undefined function inside a single DEFUN produce ONE condition, whose
source path names only the first call. Spread the same three calls across three
DEFUNs and there are three conditions, one each. So the warning's real scope is
the top-level form, and narrowing to the subform the path names would silently
drop the second and third use.

Undefined variables group the same way, and their path does not even reach a use
-- three references inside one LIST call gave a path pointing at the LIST call.

Everything else is per-site and should be narrowed: two bad-arity calls to the
same function in one form produced two conditions with two distinct paths.")

(defun diagnostics-for (extracted tree source)
       "Turn one extracted diagnostic into LSP diagnostics.

The scope to search is the narrowest region SBCL actually blamed -- which is the
subform its source path names, except for the kinds it reports per top-level form
(see +FORM-SCOPED-KINDS+). Within that scope every occurrence of the symbol is
marked, because within it every occurrence really is wrong.

Each step below is a genuine fallback, not a guess dressed up as one:

  1. Mark the symbol's token(s) inside the scope. This is the error and nothing
     but the error.
  2. The symbol is not spelled in the scope -- a macroexpansion, most likely --
     so mark the scope itself.
  3. No symbol at all (reader errors, unclassified conditions): the scope.
  4. No location at all: the head of the file, since a diagnostic still has to
     be reported somewhere."
       (let* ((severity (severity-for extracted))
              (message (clef-conditions:diagnostic-message extracted))
              (sym (clef-conditions:diagnostic-symbol extracted))
              (kind (clef-conditions:diagnostic-kind extracted))
              (source-path (clef-conditions:diagnostic-source-path extracted))
              (index (toplevel-index source-path))
              (forms (toplevel-forms tree))
              (form (when (and index (< index (length forms))) (nth index forms)))
              (subform (resolve-source-path tree source-path))
              (scope (if (member kind +form-scoped-kinds+)
                         (or form subform)
                         (or subform form))))
             (flet ((diag (node)
                          (dict "range" (node-to-range node)
                                "severity" severity
                                "message" message)))
                   (cond
                     ((and scope sym)
                      (let ((nodes (symbol-nodes-in scope (symbol-name sym) source)))
                           (if nodes
                               (mapcar #'diag nodes)
                               ;; The symbol is not spelled here -- a macro
                               ;; expansion, most likely. Mark what we do have.
                               (list (diag scope)))))
                     (scope (list (diag scope)))
                     (t (list (dict "range" (make-range 0 0 0 0)
                                    "severity" severity
                                    "message" message)))))))

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
