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
            "cl-ppcre")
           :serial t
           :components ((:module "src"
                                 :components ((:file "packages")
                                              (:file "util")
                                              (:file "log")
                                              (:file "jsonrpc/types")
                                              (:file "jsonrpc/messages")
                                              (:file "lsp/types/base/types")
                                              (:file "lsp/types/base/error-codes")
                                              (:file "lsp/types/basic/position")
                                              ;; (:file "lsp/types/lifecycle/initialize-params")
                                              (:file "lsp/server-capabilities")
                                              (:file "lsp/server")
                                              ;; (:file "lsp/defhandler")
                                              (:file "lsp/lifecycle/initialize")
                                              (:file "lsp/lifecycle/initialized")
                                              (:file "lsp/document/did-open")
                                              (:file "lsp/document/did-change")

                                              ;; Alive files, for formatting
                                              ;; TODO: Extract this into a different system
                                              (:file "alive/utils")
                                              (:file "alive/types")
                                              (:file "alive/errors")
                                              (:file "alive/position")
                                              (:file "alive/range")
                                              (:file "alive/text-edit")
                                              (:file "alive/parse/token")
                                              (:file "alive/parse/tokenizer")
                                              (:file "alive/parse/form")
                                              (:file "alive/parse/forms")
                                              (:file "alive/compat/sbcl/symbols")
                                              (:file "alive/packages")
                                              (:file "alive/symbols")
                                              (:file "alive/format")

                                              (:file "lsp/document/formatting")
                                              (:file "main")))))
