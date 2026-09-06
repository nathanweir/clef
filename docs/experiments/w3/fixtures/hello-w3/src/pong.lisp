;;; The other half of the cycle. See ping.lisp.
(defpackage :hello-w3/src/pong
  (:use :cl)
  (:local-nicknames (:ping :hello-w3/src/ping)))

(in-package :hello-w3/src/pong)
