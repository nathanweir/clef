(in-package :clef-lsp/defhandler)

;; (defmacro while (condition &body body)
;;   `(loop while ,condition do (progn ,@body)))

;; (jsonrpc:expose *server* "sum" (lambda (args) (reduce #'+ args)))

(defmacro defhandler (endpoint-name handler-lambda)
    "Defines an LSP handler for the given endpoint name."
    `(jsonrpc:expose *server* ,endpoint-name
                     (lambda (args)
                         ;; From src/lsp/server.lisp
                         (before-handle-request ,endpoint-name args)
                         (funcall ,handler-lambda args))))
