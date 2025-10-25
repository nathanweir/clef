(defsystem :clef
           :description "Common Lisp Editor Facilitator - An LSP server for Common Lisp"
           :author "Nathan Weir"
           :license "MIT"
           :version "0.0.1"
           :depends-on ("jsonrpc" "serapeum")
           :serial t
           :components ((:module "src"
                                 :components ((:file "packages")
                                              (:file "lsp/types/base/types")
                                              (:file "lsp/types/base/error-codes")
                                              (:file "lsp/types/basic/position")
                                              (:file "lsp/server")
                                              (:file "lsp/defhandler")
                                              (:file "lsp/lifecycle/initialize")
                                              (:file "main")))))
