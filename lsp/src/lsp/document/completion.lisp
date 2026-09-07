(defpackage :clef-lsp/src/lsp/document/completion
  (:use :cl)
  (:import-from :clef-lsp/src/log #:slog)
  (:import-from :serapeum #:dict #:href)
  (:local-nicknames
    (:base :clef-lsp/src/lsp/types/base/types)
    (:ctx :clef-lsp/src/context)
    (:repair :clef-lsp/src/parser/repair)
    (:rpc :clef-lsp/src/jsonrpc/types)
    (:sym :clef-lsp/src/symbols/types)
    (:symbols :clef-lsp/src/symbols/init))
  (:export
   #:handle-text-document-completion))

(in-package :clef-lsp/src/lsp/document/completion)

;;;; textDocument/completion.
;;;;
;;;; The endpoint with the least margin for sloppiness, because it is the only
;;;; one invoked on every keystroke and the only one whose input is *always*
;;;; syntactically incomplete.
;;;;
;;;; What it used to do, measured in
;;;; docs/experiments/lsp/09-completion-scenarios.lisp: return every symbol in
;;;; the global scope, unfiltered, for any prefix at all -- 979 items for `(zzzz'
;;;; -- while omitting the LET bindings and parameters in scope at the cursor,
;;;; which are the only candidates a language server can offer that a plain
;;;; word-completer cannot. Two of eleven realistic scenarios answered usefully,
;;;; and both of those were cases where the right answer is "nothing".
;;;;
;;;; Three separate causes, fixed in three places:
;;;;
;;;;   - the missing lexical bindings were not a completion bug at all. An
;;;;     unclosed form collapsed the parse and the file indexed to an empty
;;;;     document scope. Fixed in the indexer -- see
;;;;     clef-lsp/src/parser/repair:repair-source.
;;;;   - no prefix filtering whatsoever. Fixed here.
;;;;   - nothing offered at an empty head position `(', because the handler
;;;;     required a symbol reference to already exist at the cursor. Fixed here:
;;;;     an empty prefix is a valid prefix.

;;; ---------------------------------------------------------------------------
;;; Locating the cursor in the text
;;; ---------------------------------------------------------------------------

(defparameter +completion-item-limit+ 200
  "How many candidates to return at most.

The global scope holds every symbol of every loaded system -- 978 before a
project defines anything. Shipping all of them on every keystroke is what the
`isIncomplete' flag exists to avoid: past this many we truncate and set it, and
the client re-queries as the prefix narrows.")

(defun character-offset (text line character)
  "The offset into TEXT of 0-indexed LINE and CHARACTER, clamped to TEXT.

Character offset, not byte offset: this indexes the document string directly, so
it must count characters. The two coincide for ASCII and diverge the moment a
file has a non-ASCII identifier or string in it."
  (let ((offset 0)
        (n (length text)))
    (dotimes (i line)
      (declare (ignore i))
      (let ((nl (position #\Newline text :start offset)))
        (if nl (setf offset (1+ nl)) (return))))
    (min n (+ offset character))))

(defun symbol-constituent-p (c)
  "Is C part of a symbol token being typed?

The complement of whitespace and the terminating macro characters. Colon is
deliberately included so that a package-qualified prefix arrives whole."
  (not (or (member c '(#\Space #\Tab #\Newline #\Return #\Page))
           (member c '(#\( #\) #\' #\" #\; #\` #\, )))))

(defun prefix-at (text offset)
  "The symbol token being typed immediately before OFFSET, possibly empty.

Read from the text rather than from the symbol index, deliberately. The index
describes the last state the file could be parsed in; the prefix is whatever the
user has typed since, which is exactly the part the index cannot know."
  (let ((start offset))
    (loop while (and (plusp start)
                     (symbol-constituent-p (char text (1- start))))
          do (decf start))
    (subseq text start offset)))

(defun split-qualified (prefix)
  "Split PREFIX into (values package-part name-part separator).

`cl:for' -> \"cl\", \"for\", \":\" and `cl::for' -> \"cl\", \"for\", \"::\".
An unqualified prefix gives NIL for the package part. A leading colon means a
keyword, which is a package designator of its own kind and is handled by the
caller rather than treated as a package named \"\"."
  (let ((colon (position #\: prefix)))
    (cond
      ((null colon) (values nil prefix nil))
      ((zerop colon) (values nil prefix nil))
      (t (let* ((double (and (< (1+ colon) (length prefix))
                             (char= (char prefix (1+ colon)) #\:)))
                (sep (if double "::" ":")))
           (values (subseq prefix 0 colon)
                   (subseq prefix (+ colon (length sep)))
                   sep))))))

;;; ---------------------------------------------------------------------------
;;; Candidates
;;; ---------------------------------------------------------------------------

(defun determine-symbol-kind (symbol-def)
  "Determine the LSP CompletionItemKind for SYMBOL-DEF."
  (let ((kind (sym:symbol-definition-kind symbol-def)))
    (cond
      ((eq kind :function) base:+completion-item-kind-function+)
      ((eq kind :macro) base:+completion-item-kind-function+)
      ((eq kind :variable) base:+completion-item-kind-variable+)
      ((eq kind :class) base:+completion-item-kind-class+)
      ((eq kind :package) base:+completion-item-kind-module+)
      ((eq kind :constant) base:+completion-item-kind-constant+)
      ((eq kind :type) base:+completion-item-kind-type-parameter+)
      (t base:+completion-item-kind-text+))))

(defun prefix-match-p (candidate prefix)
  "Does CANDIDATE start with PREFIX, case-insensitively?

An empty PREFIX matches everything, which is what makes an empty head position
`(' offer the whole scope rather than nothing."
  (let ((pl (length prefix)))
    (and (<= pl (length candidate))
         (string-equal candidate prefix :end1 pl))))

(defun scope-candidates (scope prefix)
  "Definitions in SCOPE whose names match PREFIX, innermost scope's own names.

Lambda-list markers are dropped. They are interned symbols and so live in the
global scope like anything else, but `&body' is never a thing you are trying to
call, and they crowded the top of every list."
  (loop for def in (sym:lexical-scope-symbol-definitions scope)
        for name = (sym:symbol-definition-symbol-name def)
        when (and (prefix-match-p name prefix)
                  (not (and (plusp (length name)) (char= (char name 0) #\&))))
          collect def))

(defun keyword-candidates (prefix)
  "Keyword symbols matching PREFIX, for a prefix that began with a colon.

The image is the only source for these -- keywords are not definitions and so
appear in no scope. Deliberately capped, because :KEYWORD in a long-running
image accumulates a great many symbols."
  (let ((out '())
        (n 0))
    (do-symbols (s (find-package :keyword))
      (let ((name (string-downcase (symbol-name s))))
        (when (and (prefix-match-p name prefix)
                   (not (member name out :test #'string=)))
          (push name out)
          (when (> (incf n) +completion-item-limit+) (return)))))
    (sort out #'string<)))

(defun completion-item (label kind)
  (dict "label" label "kind" kind))

;;; ---------------------------------------------------------------------------
;;; Handler
;;; ---------------------------------------------------------------------------

(defun handle-text-document-completion (message)
  "Handle a textDocument/completion request."
  (let* ((params (rpc:request-params message))
         (document-uri (href params "text-document" "uri"))
         (line (href params "position" "line"))
         (character (href params "position" "character"))
         (text (gethash document-uri ctx:documents)))
    (slog :debug "Completion request for URI: ~A at line ~A, character ~A"
          document-uri line character)
    (if (null text)
        (dict "isIncomplete" nil "items" #())
        (let* ((offset (character-offset text line character))
               ;; The same lexer the indexer's repair pass uses, run over the
               ;; text up to the cursor. Inside a string or a comment there is
               ;; nothing to complete, and offering symbols there is actively
               ;; annoying -- it fires while you are writing prose.
               (state (repair:scan-source (subseq text 0 offset)))
               (in-inert-context
                 (or (repair:scan-in-string state)
                     (repair:scan-in-line-comment state)
                     (plusp (repair:scan-block-comment-depth state))))
               (prefix (prefix-at text offset)))
          (cond
            (in-inert-context (dict "isIncomplete" nil "items" #()))

            ;; A keyword: :in|itial-element. Its own namespace entirely.
            ((and (plusp (length prefix)) (char= (char prefix 0) #\:))
             (let* ((bare (string-left-trim ":" prefix))
                    (names (keyword-candidates bare))
                    (truncated (> (length names) +completion-item-limit+))
                    (kept (if truncated (subseq names 0 +completion-item-limit+) names)))
               (dict "isIncomplete" truncated
                     "items" (map 'vector
                                  (lambda (n)
                                    (completion-item
                                     (format nil ":~A" n)
                                     base:+completion-item-kind-constant+))
                                  kept))))

            (t
             (multiple-value-bind (package-part name-part separator)
                 (split-qualified prefix)
               (multiple-value-bind (ref-name ref-scope)
                   (symbols:get-ref-for-doc-pos document-uri line character)
                 (declare (ignore ref-name))
                 (let ((seen (make-hash-table :test #'equal))
                       (items (make-array 0 :adjustable t :fill-pointer 0))
                       (truncated nil))
                   (loop for scope = ref-scope
                           then (sym:lexical-scope-parent-scope scope)
                         while (and scope (not truncated))
                         do (dolist (def (scope-candidates scope name-part))
                              (let* ((name (sym:symbol-definition-symbol-name def))
                                     ;; Give the qualification back, so that the
                                     ;; text the client sees still starts with
                                     ;; what the user typed -- otherwise its own
                                     ;; filtering discards every candidate.
                                     (label (if package-part
                                                (format nil "~A~A~A" package-part separator name)
                                                name)))
                                (unless (gethash label seen)
                                  (setf (gethash label seen) t)
                                  (vector-push-extend
                                   (completion-item label (determine-symbol-kind def))
                                   items)
                                  (when (>= (length items) +completion-item-limit+)
                                    (setf truncated t)
                                    (return))))))
                   (slog :debug "[completion] prefix ~S -> ~D item(s)~:[~; (truncated)~]"
                         prefix (length items) truncated)
                   (dict "isIncomplete" truncated "items" items))))))))))
