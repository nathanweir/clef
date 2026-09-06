;;; Half of a deliberate import cycle with pong.lisp. Loaded only by
;;; experiment E2c, to see what a circular package dependency produces.
(defpackage :hello-w3/src/ping
  (:use :cl)
  (:local-nicknames (:pong :hello-w3/src/pong)))

(in-package :hello-w3/src/ping)
