(defpackage :clef-lsp/src/lsp/lifecycle/initialized
  (:use :cl)
  (:local-nicknames
    (:ctx :clef-lsp/src/context))
  (:export
   #:handle-initialized))

(in-package :clef-lsp/src/lsp/lifecycle/initialized)

(defun handle-initialized (request)
       (declare (ignore request))
       (setf ctx:initialized t)
       ;; Send no response
       nil)
