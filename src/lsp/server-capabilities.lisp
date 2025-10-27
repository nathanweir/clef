(in-package :clef-lsp/server)

;; (defvar *capabilities*
;;         '(("capabilities"
;;            . (("general"
;;                . (("positionEncodings"
;;                    . ("utf-8" "utf-32" "utf-16"))))
;;               ("textDocument"
;;                . (("codeAction"
;;                    . (("codeActionLiteralSupport"
;;                        . (("codeActionKind"
;;                            . (("valueSet"
;;                                . ("" "quickfix" "refactor" "refactor.extract"
;;                                      "refactor.inline" "refactor.rewrite" "source"
;;                                      "source.organizeImports"))))))
;;                       ("dataSupport" . (T))
;;                       ("disabledSupport" . (T))
;;                       ("isPreferredSupport" . (T))
;;                       ("resolveSupport"
;;                        . (("properties"
;;                            . ("edit" "command"))))))))))))

;;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities

(defvar *server-capabilities-json*
        (dict "textDocumentSync" (dict "change" 1)
              "documentFormattingProvider" t))
;; (format t "json version is: ~A" (com.inuoe.jzon:parse (com.inuoe.jzon:stringify *capabilities*)))
