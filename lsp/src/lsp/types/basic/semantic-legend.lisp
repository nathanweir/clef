(in-package :clef-lsp/types/basic)

;;;; The semantic tokens legend.
;;;;
;;;; Here rather than beside the handler because the legend is sent twice over:
;;;; once in the server capabilities, so the client knows what the indices mean,
;;;; and once implicitly in every token the handler encodes. Those two must agree
;;;; exactly -- an index off by one recolours the whole file -- and the only way
;;;; to guarantee that is for there to be one list.
;;;;
;;;; server-capabilities.lisp loads before any handler, so this module (which
;;;; already holds NODE-TO-RANGE and the SymbolKind mapping for the same reason)
;;;; is where both can reach it.

(defparameter *semantic-token-types*
  #("keyword" "function" "macro" "variable" "parameter" "type" "class"
    "property" "string" "number" "comment" "namespace" "method" "struct")
  "The token type legend, in index order.

Values are from the spec's SemanticTokenTypes enumeration -- all 23 of them are
listed in metaModel.json. A server may use a subset, but inventing a name means
clients with no theme rule for it render nothing at all.

NAMESPACE, METHOD and STRUCT are appended rather than inserted. The legend is an
index-ordered contract and renumbering it would recolour every existing token.

They exist because the indexer already told these apart and the legend threw it
away: :METHOD collapsed into `function', :STRUCT into `type', and :PACKAGE into
nothing at all -- so a DEFPACKAGE name was reported as `variable'. Common Lisp
has more namespaces than most languages, and this legend is where a client finds
out about them.")

(defparameter *semantic-token-modifiers*
  #("definition" "readonly" "defaultLibrary")
  "The modifier legend. Sent as a bitset of indices into this.

DEFAULTLIBRARY is the interesting one for Common Lisp: it is what lets a theme
show LIST the standard function differently from LIST the variable you just
bound, which no grammar can distinguish.")

(defun semantic-token-type-index (name)
  "The legend index for NAME, or 0 if it is not in the legend.

Falling back to 0 rather than erroring: a token with the wrong colour is a
cosmetic problem, and a handler that dies mid-file is not.

CL:POSITION, spelled out. This package does (:shadow :position) so that it can
name the LSP Position class, which means a bare POSITION here is that class and
not the sequence function -- and the failure is UNDEFINED-FUNCTION at run time,
not a compile error. The package's own definition carries a TODO asking \"Just
how dangerous is this?\"; this is the answer."
  (or (cl:position name *semantic-token-types* :test #'string=) 0))

(defun semantic-token-modifier-bit (name)
  (let ((index (cl:position name *semantic-token-modifiers* :test #'string=)))
    (if index (ash 1 index) 0)))
