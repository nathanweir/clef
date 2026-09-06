;;; The entire manifest, permanently.
;;;
;;; :class :package-inferred-system makes ASDF derive both the component list
;;; and the dependency list from the DEFPACKAGE form at the top of each file.
;;; There is nothing here to regenerate when files or dependencies change --
;;; which is the point of the W3 experiment this fixture belongs to.
(asdf:defsystem "hello-w3"
  :class :package-inferred-system
  :depends-on ("hello-w3/src/main"))
