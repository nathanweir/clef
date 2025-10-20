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
    '(or lspobject
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
