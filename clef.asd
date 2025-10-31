(defsystem :clef
           :description "Common Lisp Editor Facilitator - An LSP server for Common Lisp"
           :author "Nathan Weir"
           :license "MIT"
           :version "0.0.1"
           :depends-on
           ("uiop"
            "babel"
            "com.inuoe.jzon"
            "serapeum"
            "cl-change-case"
            "cl-indentify"
            "cl-ppcre"
            "bordeaux-threads"
            "cl-tree-sitter")
           :serial t
           :components ((:module "src"
                                 :components ((:file "packages")
                                              (:file "util")
                                              (:file "log")
                                              (:file "jsonrpc/types")
                                              (:file "jsonrpc/messages")
                                              (:file "parser/parser")
                                              (:file "lsp/types/base/types")
                                              (:file "lsp/types/base/error-codes")
                                              (:file "lsp/types/basic/position")
                                              ;; (:file "lsp/types/lifecycle/initialize-params")
                                              (:file "lsp/server-capabilities")
                                              (:file "lsp/server")
                                              ;; (:file "lsp/defhandler")
                                              (:file "lsp/lifecycle/initialize")
                                              (:file "lsp/lifecycle/initialized")
                                              (:file "lsp/document/diagnostic")
                                              (:file "lsp/document/did-open")
                                              (:file "lsp/document/did-change")
                                              (:file "lsp/document/formatting")
                                              (:file "lsp/workspace/diagnostic")
                                              (:file "lsp/workspace/did-change-configuration")
                                              (:file "lsp/misc/shutdown")
                                              (:file "lsp/misc/exit")
                                              (:file "main")))))
