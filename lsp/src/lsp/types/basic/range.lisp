(in-package :clef-lsp/types/basic)

;;; LSP Position and Range, as the plain dicts that actually go on the wire.
;;;
;;; These live here rather than beside a handler because three copies of the
;;; node->Range conversion had accumulated -- one in lsp/document/definition,
;;; one in lsp/document/diagnostic, one in lsp/workspace/symbol -- and they had
;;; drifted. This is the module that loads before every handler, so it is the
;;; one place all of them can share.
;;;
;;; Note this is deliberately NOT the CLOS treatment the POSITION class in
;;; position.lisp gets. Replicating the spec's types as classes was abandoned;
;;; these are dict builders, and naming them MAKE-* keeps that honest.
;;;
;;; On orientation, which is the whole reason to have one definition:
;;; cl-tree-sitter's NODE-RANGE returns ((col row) (col row)) -- column first.
;;; CLEF-PARSER/PARSER:NODE-RANGE normalises that to the line-first order LSP
;;; wants. Destructuring either one by hand at a call site is what transposed
;;; line and character in every syntax-error diagnostic for as long as
;;; GET-SYNTAX-ERRORS built its own ranges. Call NODE-TO-RANGE instead.

(defun make-position (line char)
       "An LSP Position dict. LINE and CHAR are both zero-based."
       (dict "line" line "character" char))

(defun make-range (start-line start-char end-line end-char)
       "An LSP Range dict. Line before character, at both ends."
       (dict "start" (make-position start-line start-char)
             "end" (make-position end-line end-char)))

(defun node-to-range (node)
       "The LSP Range covering tree-sitter NODE, or NIL if NODE is NIL."
       (when node
             (multiple-value-bind (start-line start-char end-line end-char)
                                  (clef-parser/parser:node-range node)
                                  (make-range start-line start-char
                                              end-line end-char))))
