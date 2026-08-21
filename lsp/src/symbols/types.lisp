(in-package :clef-symbols)

;; Custom struct for adding data to the :cl-interval interval tree
(defstruct (clef-interval (:include interval:interval))
           data)

;; TODO: No idea how much this will be used or what's appropriate
(defparameter +symbol-kinds+ '(:variable :function :macro :class :package :constant :type)
              "Enumeration of possible kinds of symbols.")

(deftype symbol-kind ()
         "An enum type for symbol kinds.

:STRUCT and :METHOD were missing even though LISP-KIND-TO-LSP-KIND already
mapped both -- so recording either would have been a type error under safety,
which is why nothing did."
         `(member :unknown :variable :function :macro :class :struct :method
                  :package :constant :type :special-operator))

(defparameter +scope-kinds+ '(:let :flet :labels :lambda :defun :defmacro)
              "Enumeration of possible kinds of scope bindings.")

(deftype scope-kind ()
         "An enum type for scope kinds."
         `(member :workspace :document :let :flet :labels :lambda :defun :defmacro))

(defstruct location
           "A range (character offset) location within a file of source code."
           (file-path nil :type string)
           ;; The index of the start character of the range
           (start nil :type integer)
           ;; The index of the end character of the range
           (end nil :type integer))
;; TODO: Bring these back in if it ends up being more convenient to track the original
;; start & end line/char than it is to recalculate at time of use
;; These line & character vals are translated into start/end above upon creation
;; Original line number for position in the source doc
;; (start-line nil :type integer)
;; ;; Original character index for position in the source doc
;; (start-character nil :type integer)
;; (end-line nil :type integer)
;; (end-character nil :type integer))

(defstruct symbol-definition
           "A definition of a symbol in the workspace."
           (symbol-name nil :type string)
           (package-name nil :type symbol)
           (kind nil :type symbol-kind)
           ;; Shouldn't be null for a local file but likely will be for built-ins or external references
           (location nil :type (or null location))
           (defining-scope nil :type lexical-scope)
           ;; The AST node. TODO: This might be an AWFUL idea
           (node nil)
           ;; The node of the whole defining FORM, where NODE is just the name.
           ;;
           ;; (defun foo (x) ...)
           ;;  ^--------------^   form-node
           ;;         ^-^         node
           ;;
           ;; Recorded at index time rather than recovered later, because
           ;; recovering it from the scope interval tree does not work: a scope
           ;; whose extent is identical to an existing one is silently dropped by
           ;; the tree, so a file consisting of a single top-level DEFUN has no
           ;; DEFUN scope at all. See docs/surveys/lsp-review.md §1.8.
           ;;
           ;; textDocument/documentSymbol needs this to give a DocumentSymbol a
           ;; `range' covering the whole definition while `selectionRange' covers
           ;; the name -- which is what puts "inside FOO" in an editor breadcrumb.
           (form-node nil))

(defstruct symbol-reference
           "A reference (usage) of a symbol in the workspace."
           (symbol-name nil :type string)
           ;; The package in effect where the reference appears -- which is what
           ;; the commented-out version of this slot asked about: "I think it'd
           ;; be the package that's current at time of use." That is right, and
           ;; it is necessary.
           ;;
           ;; Without it the workspace index is consulted by bare name and the
           ;; first match wins, so go-to-definition on DIAGNOSTIC-SEVERITY in
           ;; conditions/src/render.lisp landed on a same-named test helper in an
           ;; unrelated component. See docs/surveys/lsp-review.md §3c.2.
           (package-name nil :type symbol)
           (location nil :type location)
           (usage-scope nil :type lexical-scope)
           (node nil))
;; TODO: Could pre-compute this, for but now probably easier to just calculate by walking up the tree
;; at time of need.
;; nil if not resolvable
;; (resolved-definition nil :type (or null symbol-definition)))

(defstruct lexical-scope
           (kind nil :type scope-kind)
           ;; If null, then this scope is workspace-specific and not file-specific
           (location nil :type (or null location))
           ;; If null then this scope is the root of the scope tree
           (parent-scope nil :type (or null lexical-scope))
           ;; Should be a list of symbol-definition's
           (symbol-definitions nil :type list)
           (symbol-references nil :type hash-table)
           ;; Should be a list of lexical-scope's
           (child-scopes nil :type list)
           (node nil))

(defstruct system-info
  "Information about an ASDF system discovered in the workspace."
  (name nil :type (or null string))
  (asd-path nil :type (or null string))
  (dependencies nil :type list)
  (source-files nil :type list)
  (loaded-p nil :type boolean))
