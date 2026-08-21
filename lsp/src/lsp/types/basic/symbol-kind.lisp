(in-package :clef-lsp/types/basic)

;;; LSP SymbolKind, and the mapping from clef's own symbol kinds onto it.
;;;
;;; Here rather than beside a handler for the same reason NODE-TO-RANGE is: more
;;; than one handler reports symbols, and a second copy of a lookup table is how
;;; the two drift. That is not hypothetical in this codebase -- the JSON-RPC
;;; MethodNotFound code was re-typed rather than referenced and came out as
;;; +32601 instead of -32601, which no test noticed for as long as the code
;;; existed.
;;;
;;; Values are from the LSP 3.17 specification's SymbolKind enumeration.

(defconstant +symbol-kind-file+ 1)
(defconstant +symbol-kind-module+ 2)
(defconstant +symbol-kind-namespace+ 3)
(defconstant +symbol-kind-package+ 4)
(defconstant +symbol-kind-class+ 5)
(defconstant +symbol-kind-method+ 6)
(defconstant +symbol-kind-property+ 7)
(defconstant +symbol-kind-field+ 8)
(defconstant +symbol-kind-constructor+ 9)
(defconstant +symbol-kind-enum+ 10)
(defconstant +symbol-kind-interface+ 11)
(defconstant +symbol-kind-function+ 12)
(defconstant +symbol-kind-variable+ 13)
(defconstant +symbol-kind-constant+ 14)
(defconstant +symbol-kind-string+ 15)
(defconstant +symbol-kind-number+ 16)
(defconstant +symbol-kind-boolean+ 17)
(defconstant +symbol-kind-array+ 18)
(defconstant +symbol-kind-object+ 19)
(defconstant +symbol-kind-key+ 20)
(defconstant +symbol-kind-null+ 21)
(defconstant +symbol-kind-enum-member+ 22)
(defconstant +symbol-kind-struct+ 23)
(defconstant +symbol-kind-event+ 24)
(defconstant +symbol-kind-operator+ 25)
(defconstant +symbol-kind-type-parameter+ 26)

(defun lisp-kind-to-lsp-kind (kind)
  "Convert clef's internal symbol kind to an LSP SymbolKind.

The enumeration was designed for languages with classes and methods, so several
Lisp kinds have no exact home. Macros report as Function, which is the least
misleading option available -- an editor showing them under a `Function' heading
is closer to true than any of Object, Key or Event.

The mapping is coarser than it should be, but that is upstream of here: the
indexer records :function for everything DEFUN-shaped and :variable for
everything DEFPARAMETER-shaped, so DEFMACRO cannot currently be told from DEFUN,
nor DEFCONSTANT from DEFVAR. See docs/surveys/lsp-review.md §1.5."
  (case kind
    (:function +symbol-kind-function+)
    (:macro +symbol-kind-function+)
    (:variable +symbol-kind-variable+)
    (:constant +symbol-kind-constant+)
    (:parameter +symbol-kind-variable+)
    (:class +symbol-kind-class+)
    (:struct +symbol-kind-struct+)
    (:type +symbol-kind-type-parameter+)
    (:package +symbol-kind-package+)
    (:method +symbol-kind-method+)
    (:special-operator +symbol-kind-operator+)
    (otherwise +symbol-kind-variable+)))
