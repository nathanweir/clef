(in-package :clef-lsp/types/base)

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseTypes

;; TODO: For now, I'm ignoring a deftype of 'integer' as this collides
;; with Common Lisp's built-in integer type, and I think they're equivalent?

(deftype uinteger ()
    "Defines an unsigned integer number in the range of 0 to 2^31 - 1."
    '(integer 0 2147483647))

;; TODO: Revisit a possible deftype for decimal

;; LSPAny: LSPObject | LSPArray | string | integer | uinteger | decimal | boolean | null

(deftype lspany ()
    "An LSP value: object, array, string, integer, uinteger, decimal, boolean, or null."
    '(member lspobject
             lsparray
             string
             integer
             uinteger
             float
             boolean
             null))

;; LSPObject: { [key: string]: LSPAny }
(deftype lspobject ()
    "An LSP object: a hash-table mapping strings to LSPAny."
    '(hash-table))

;; LSPArray: LSPAny[]
(deftype lsparray ()
    "An LSP array: a vector of LSPAny."
    '(vector lspany))

(deftype document-uri ()
    "https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri"
    'string)

;;; CompletionItemKind
;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
(defconstant +completion-item-kind-text+ 1)
(defconstant +completion-item-kind-method+ 2)
(defconstant +completion-item-kind-function+ 3)
(defconstant +completion-item-kind-constructor+ 4)
(defconstant +completion-item-kind-field+ 5)
(defconstant +completion-item-kind-variable+ 6)
(defconstant +completion-item-kind-class+ 7)
(defconstant +completion-item-kind-interface+ 8)
(defconstant +completion-item-kind-module+ 9)
(defconstant +completion-item-kind-property+ 10)
(defconstant +completion-item-kind-unit+ 11)
(defconstant +completion-item-kind-value+ 12)
(defconstant +completion-item-kind-enum+ 13)
(defconstant +completion-item-kind-keyword+ 14)
(defconstant +completion-item-kind-snippet+ 15)
(defconstant +completion-item-kind-color+ 16)
(defconstant +completion-item-kind-file+ 17)
(defconstant +completion-item-kind-reference+ 18)
(defconstant +completion-item-kind-folder+ 19)
(defconstant +completion-item-kind-enum-member+ 20)
(defconstant +completion-item-kind-constant+ 21)
(defconstant +completion-item-kind-struct+ 22)
(defconstant +completion-item-kind-event+ 23)
(defconstant +completion-item-kind-operator+ 24)
(defconstant +completion-item-kind-type-parameter+ 25)
