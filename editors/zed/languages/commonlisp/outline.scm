;; Outline query for Common Lisp

;; Match top-level function definitions
(defun_header
  function_name: (_) ) @name @item

;; Match top-level macro definitions
; (defun_header
;   keyword: (defun_keyword
;     "defmacro")
;   function_name: (_) ) @name @item

;; Match top-level class definitions
(list_lit
  .
  (sym_lit) @context
  (#eq? @context "defclass")
  .
  (sym_lit) @name)  @item
  (#strip! @item "^\(")

;; Match top-level package definitions
; (list_lit
;   value: (sym_lit) @item
;   .
;   value: (kwd_lit) @name)

;; Match other top-level constructs
(source
  (list_lit
    .
    (
    (sym_lit) @context)
    (#any-of? @context
        "defvar"
        "defparameter"
        "defconstant")
    .
    (sym_lit) @name) @item)
    (#strip! @item "^\(")
