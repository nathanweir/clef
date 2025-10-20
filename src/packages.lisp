(defpackage :clef-root
    (:use :cl)
    (:export :start-server))

(defpackage :clef-lsp/server
    (:use :cl)
    (:export :start
             :before-handle-request
             *server*))

(defpackage :clef-lsp/defhandler
    (:use :cl)
    ;; I have no idea why, but I *must* import *server* here
    ;; and use without the fully qualified name in defhandler in order to
    ;; properly reference it
    (:import-from :clef-lsp/server *server* :before-handle-request)
    (:export :defhandler))

(defpackage :clef-lsp/types/base
    (:use :cl)
    (:export :uinteger
             +server-not-initialized+))

(defpackage :clef-lsp/types/basic
    (:use :cl)
    (:import-from :clef-lsp/types/base :uinteger)
    ;; TODO: Just how dangerous is this?
    (:shadow :position)
    (:export :position
             :position-line
             :position-character))

(defpackage :clef-lsp/lifecycle
    (:use :cl :clef-lsp/defhandler))
