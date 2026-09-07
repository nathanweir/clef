;;; <%= @ app-name %>.asd
;;;
;;; This file is the whole manifest, permanently. It never lists source files
;;; and never lists dependencies -- both are derived from the DEFPACKAGE form
;;; at the top of each file (ASDF's package-inferred-system). Adding a file or
;;; a dependency means writing the import where you use it, nothing here.
;;;
;;; See docs/golden-path/packages.md in the clef repository for the
;;; convention, and docs/surveys/w3-packages.md for the measurements behind it.

(asdf:defsystem #:<%= @ app-name %>
  :description "<%= (or (@ description) "A clef golden-path application.") %>"
  :author      "<%= (or (@ author) "Your Name") %>"
  :license     "<%= (or (@ license) "MIT") %>"
  :version     "0.1.0"
  :class       :package-inferred-system
  :depends-on  ("<%= @ app-name %>/src/main")
  ;; What `clef run' calls once the system is loaded. ASDF's own field: an
  ;; executable built with program-op starts at the same function.
  :entry-point "<%= @ app-name %>/src/main:main"
  ;; What `clef test' loads before calling RUN-TESTS there. The :perform keeps
  ;; (asdf:test-op :<%= @ app-name %>) working from a REPL as well.
  :in-order-to ((asdf:test-op (asdf:load-op "<%= @ app-name %>/test/main")))
  :perform     (asdf:test-op (o c)
                 (uiop:symbol-call :<%= @ app-name %>/test/main :run-tests)))
