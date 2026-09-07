;;; clef-lsp.asd
;;;
;;; This file is the whole manifest, permanently. It never lists source files
;;; and never lists dependencies -- both are derived from the DEFPACKAGE form
;;; at the top of each file (ASDF's package-inferred-system). Adding a file or
;;; a dependency means writing the import where you use it, nothing here.
;;;
;;; See docs/golden-path/packages.md for the convention.

;; Three dependencies expose packages whose names are not their system names.
;; Inference maps a package name to a system by downcasing it, so these need
;; the escape hatch the convention documents: teach ASDF the pairing here,
;; once, and every file imports the package by its real name.
(asdf:register-system-packages "cl-interval" '(:interval))
(asdf:register-system-packages "cl-indentify" '(:indentify))
(asdf:register-system-packages "cl-tree-sitter"
                               '(:cl-tree-sitter/all
                                 :cl-tree-sitter/high-level
                                 :cl-tree-sitter/low-level))

(defsystem "clef-lsp"
  :description "Common Lisp Editor Facilitator - An LSP server for Common Lisp"
  :author "Nathan Weir"
  :license "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("clef-lsp/src/main"))
