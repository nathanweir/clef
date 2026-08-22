(in-package :clef-lsp/document)

;;;; textDocument/semanticTokens/full.
;;;;
;;;; Highlighting that knows what a symbol *is*, rather than what it looks like.
;;;;
;;;; This is the one editor feature where clef can beat a grammar outright,
;;;; because two of the distinctions matter enormously in Common Lisp and are
;;;; invisible to any regex or tree-sitter query:
;;;;
;;;;   - **macro calls versus function calls.** (with-open-file ...) and
;;;;     (open ...) are spelled identically and behave nothing alike. Knowing
;;;;     which is which requires knowing what the symbol names.
;;;;   - **the standard library versus your own code.** LIST, the CL function,
;;;;     and LIST, the local you just bound, look the same. The `defaultLibrary'
;;;;     modifier separates them.
;;;;
;;;; Plus what clef already knows from its own index: which symbols are
;;;; definitions, which are parameters, which are lexical bindings.
;;;;
;;;; **Conservative on purpose.** A semantic token *overrides* the grammar's
;;;; highlighting for that range, so emitting a wrong type is worse than
;;;; emitting nothing -- the editor's own highlighting is decent and stays put
;;;; where we say nothing. Symbols that resolve to nothing get no token.

;;; ---------------------------------------------------------------------------
;;; Classification
;;; ---------------------------------------------------------------------------

(defun kind-token-type (kind)
  "The token type for one of clef's own symbol kinds."
  (case kind
    (:macro "macro")
    ((:function :method) "function")
    (:class "class")
    ((:struct :type) "type")
    ((:variable :constant) "variable")
    (:special-operator "keyword")
    (t nil)))

(defun lexical-token-type (definition)
  "PARAMETER for something bound by a lambda list, VARIABLE otherwise.

Editors theme the two differently and the distinction is real: a parameter is
part of the interface, a LET binding is not."
  (let ((scope (clef-symbols:symbol-definition-defining-scope definition)))
    (if (and scope
             (member (clef-symbols:lexical-scope-kind scope)
                     '(:defun :defmacro :lambda :flet :labels)))
        "parameter"
        "variable")))

(defun binding-scope-p (scope)
  "Is SCOPE a binding form rather than a file or the workspace?"
  (and scope
       (member (clef-symbols:lexical-scope-kind scope)
               '(:let :flet :labels :lambda :defun :defmacro))
       t))

(defun definition-token-type (definition)
  "The token type for a definition.

A binding form's contents are checked FIRST. Going by the recorded kind alone
typed every parameter as `variable', because that is the kind the indexer
records for one -- and a parameter is not the same thing as a global, which is
the whole reason the legend has both."
  (let ((scope (clef-symbols:symbol-definition-defining-scope definition)))
    (if (binding-scope-p scope)
        (lexical-token-type definition)
        (or (kind-token-type (clef-symbols:symbol-definition-kind definition))
            "variable"))))

(defun image-token-type (name package-designator)
  "Classify NAME by asking the running image. Returns (values type modifiers).

The last resort, and the only one that can speak for Common Lisp itself. Symbols
the image does not have get no token at all rather than a guess."
  (let ((sym (ignore-errors (lookup-symbol name package-designator))))
    (when sym
      (let ((builtin (if (eq (symbol-package sym) (find-package :common-lisp))
                         (semantic-token-modifier-bit "defaultLibrary")
                         0)))
        (cond
          ;; Order matters, and FBOUNDP comes before FIND-CLASS deliberately.
          ;; A great many Common Lisp symbols are both a function and a type --
          ;; LIST, STRING, SYMBOL, ARRAY -- and checking the class first typed
          ;; every call to LIST as a class. The overwhelmingly common use of a
          ;; symbol that is both is as the function.
          ((special-operator-p sym) (values "keyword" builtin))
          ((macro-function sym) (values "macro" builtin))
          ((fboundp sym) (values "function" builtin))
          ((find-class sym nil) (values "class" builtin))
          ((boundp sym)
           (values "variable"
                   (logior builtin
                           (if (constantp sym) (semantic-token-modifier-bit "readonly") 0))))
          (t (values nil 0)))))))

;;; ---------------------------------------------------------------------------
;;; Collection
;;; ---------------------------------------------------------------------------

(defun single-line-node-p (node)
  "Does NODE begin and end on the same line?

A semantic token carries one length and cannot span lines, so a multi-line
string or comment must be skipped rather than encoded wrongly. The spec is
explicit that tokens are per-line."
  (= (clef-parser/parser:node-start-point-row node)
     (clef-parser/parser:node-end-point-row node)))

(defun make-token (node type modifiers &optional length)
  "A token as (line char length type-index modifiers), or NIL.

LENGTH overrides the node's own extent, which is needed for comments: the
grammar ends a comment node at column 0 of the FOLLOWING line, so measuring it
from the node makes every comment look multi-line and the single-line guard
throws them all away."
  (when (and node type (or length (single-line-node-p node)))
    (let* ((line (clef-parser/parser:node-start-point-row node))
           (start (clef-parser/parser:node-start-point-column node))
           (width (or length
                      (- (clef-parser/parser:node-end-point-column node) start))))
      (when (plusp width)
        (list line start width (semantic-token-type-index type) modifiers)))))

(defun literal-tokens (root lines)
  "Tokens for comments, strings, numbers and keyword literals."
  (let ((tokens '()))
    (labels ((walk (node)
               (when node
                 (let ((type (case (clef-symbols:node-kind-of node)
                               (:comment "comment")
                               (:block-comment "comment")
                               (:str-lit "string")
                               (:char-lit "string")
                               (:num-lit "number")
                               ;; :initarg and friends. PROPERTY is what most
                               ;; themes colour for a keyword-ish literal.
                               (:kwd-lit "property")
                               (t nil))))
                   (if type
                       (let* ((row (clef-parser/parser:node-start-point-row node))
                              (col (clef-parser/parser:node-start-point-column node))
                              ;; A comment runs to the end of its own line.
                              (width (when (and (string= type "comment")
                                                (< row (length lines)))
                                       (- (length (aref lines row)) col)))
                              (token (make-token node type 0 width)))
                         (when token (push token tokens)))
                       ;; Only descend when this node is not itself a token --
                       ;; a :KWD-LIT contains a :KWD-SYMBOL covering the same
                       ;; text, and two tokens over one range is invalid.
                       (dolist (child (ts:node-children node))
                         (walk child)))))))
      (walk root))
    tokens))

(defun definition-tokens (file-path)
  "Tokens for the name node of every definition clef has indexed for the file."
  (let ((tokens '())
        (tree (gethash file-path ctx:lexical-scopes)))
    (when tree
      (dolist (interval (get-all-intervals-from-tree tree))
        (let ((scope (clef-symbols::clef-interval-data interval)))
          (when scope
            (dolist (def (clef-symbols:lexical-scope-symbol-definitions scope))
              (let* ((node (clef-symbols:symbol-definition-node def))
                     (type (definition-token-type def))
                     (token (make-token node type (semantic-token-modifier-bit "definition"))))
                (when token (push token tokens))))))))
    tokens))

(defun reference-tokens (file-path package-designator)
  "Tokens for every symbol reference clef has indexed for the file."
  (let ((tokens '())
        (tree (gethash file-path ctx:symbol-refs)))
    (when tree
      (dolist (interval (get-all-intervals-from-tree tree))
        (let ((ref (clef-symbols::clef-interval-data interval)))
          (when ref
            (let* ((node (clef-symbols:symbol-reference-node ref))
                   (name (clef-symbols:symbol-reference-symbol-name ref))
                   (definition (binding-of ref file-path))
                   (type nil)
                   (modifiers 0))
              (cond
                ;; Resolved inside the workspace: clef's own index knows what
                ;; it is.
                ;;
                ;; The scope check is load-bearing. The GLOBAL scope holds an
                ;; entry for every Common Lisp symbol and every symbol of every
                ;; loaded library, and resolution walks into it -- so without
                ;; this, LET, DOLIST and INCF all matched here and were typed as
                ;; ordinary functions, losing exactly the macro-versus-function
                ;; distinction this method exists to provide. Routing those to
                ;; the image instead also gets them the defaultLibrary modifier,
                ;; which is the other distinction no grammar can make.
                ((and definition
                      (not (eq (clef-symbols:lexical-scope-kind
                                (clef-symbols:symbol-definition-defining-scope definition))
                               :workspace)))
                 (setf type (definition-token-type definition)))
                ;; Otherwise ask the image, which is the only thing that can
                ;; speak for Common Lisp itself.
                (t (multiple-value-setq (type modifiers)
                     (image-token-type name package-designator))))
              (let ((token (make-token node type modifiers)))
                (when token (push token tokens))))))))
    tokens))

;;; ---------------------------------------------------------------------------
;;; Encoding
;;; ---------------------------------------------------------------------------

(defun sort-and-dedupe-tokens (tokens)
  "Sorted by position, with at most one token per start position.

Overlapping tokens are not valid, and the same name node reaches us twice -- once
as a definition and once as a reference. The definition wins, because it is the
more specific claim; sorting is stable and definitions are placed first."
  (let ((sorted (stable-sort (copy-list tokens)
                             (lambda (a b)
                               (or (< (first a) (first b))
                                   (and (= (first a) (first b))
                                        (< (second a) (second b)))))))
        (result '())
        (previous nil))
    (dolist (token sorted (nreverse result))
      (unless (and previous
                   (= (first token) (first previous))
                   (< (second token) (+ (second previous) (third previous))))
        (push token result)
        (setf previous token)))))

(defun encode-tokens (tokens)
  "The wire format: a flat array of five-element deltas per token.

Each token is (deltaLine, deltaStartChar, length, type, modifiers). The deltas
are relative to the PREVIOUS token, and deltaStartChar is relative only when the
line delta is zero -- getting that wrong shifts every token after the first."
  (let ((data '())
        (previous-line 0)
        (previous-char 0))
    (dolist (token tokens)
      (destructuring-bind (line char length type modifiers) token
        (let ((delta-line (- line previous-line)))
          (push delta-line data)
          (push (if (zerop delta-line) (- char previous-char) char) data)
          (push length data)
          (push type data)
          (push modifiers data)
          (setf previous-line line
                previous-char char))))
    (coerce (nreverse data) 'vector)))

(defun handle-text-document-semantic-tokens-full (message)
  "Handle a textDocument/semanticTokens/full request."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (text (gethash document-uri ctx:documents))
         (file-path (clef-util:cleanup-path document-uri)))
    (slog :debug "[semanticTokens/full] Document: ~A" document-uri)
    (if (null text)
        (dict "data" #())
        (let* ((tree (clef-parser/parser:parse-string text))
               (lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
               (package-designator
                 (let ((pkg (clef-parser/utils:find-package-declaration tree text)))
                   (when pkg (package-name pkg))))
               (tokens (sort-and-dedupe-tokens
                        (append (definition-tokens file-path)
                                (reference-tokens file-path package-designator)
                                (literal-tokens tree lines)))))
          (slog :debug "[semanticTokens/full] ~A token(s)" (length tokens))
          (dict "data" (encode-tokens tokens))))))
